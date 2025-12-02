using System;
using System.Collections.Generic;
using System.Linq;
using Autodesk.AutoCAD.Runtime;
using Autodesk.AutoCAD.ApplicationServices;
using Autodesk.AutoCAD.EditorInput;
using Autodesk.AutoCAD.DatabaseServices;
using Autodesk.AutoCAD.Geometry;
using System.Windows;
using AcadApp = Autodesk.AutoCAD.ApplicationServices.Application;

[assembly: CommandClass(typeof(_3D_Poly_and_Offset.Plugin))]

namespace _3D_Poly_and_Offset
{
    public class Plugin
    {
        [CommandMethod("3DPOLYLINE")]
        public void Create3DPolyline()
        {
            Document doc = AcadApp.DocumentManager.MdiActiveDocument;
            Editor ed = doc.Editor;
            Database db = doc.Database;

            try
            {
                // TEXT 엔티티 선택
                PromptSelectionOptions pso = new PromptSelectionOptions
                {
                    MessageForAdding = "\n숫자 텍스트를 선택하세요: "
                };
                SelectionFilter filter = new SelectionFilter(new[]
                {
                    new TypedValue((int)DxfCode.Start, "TEXT")
                });

                PromptSelectionResult selRes = ed.GetSelection(pso, filter);
                if (selRes.Status != PromptStatus.OK)
                {
                    ed.WriteMessage("\n선택이 취소되었습니다.");
                    return;
                }

                List<Point3d> points = new List<Point3d>();

                using (Transaction tr = db.TransactionManager.StartTransaction())
                {
                    foreach (SelectedObject selObj in selRes.Value)
                    {
                        if (selObj == null) continue;
                        DBText text = tr.GetObject(selObj.ObjectId, OpenMode.ForRead) as DBText;
                        if (text == null) continue;

                        string txt = text.TextString;
                        if (double.TryParse(txt, out double z))
                        {
                            Point3d pt = text.Position;
                            points.Add(new Point3d(pt.X, pt.Y, z));
                        }
                    }
                    tr.Commit();
                }

                if (points.Count < 3)
                {
                    ed.WriteMessage("\n최소 3개의 숫자 텍스트가 필요합니다.");
                    return;
                }

                // 시작점 선택
                ed.WriteMessage("\n시작점을 선택하세요: ");
                PromptPointOptions ppo = new PromptPointOptions("\n시작점을 선택하세요: ");
                PromptPointResult ppr = ed.GetPoint(ppo);
                if (ppr.Status != PromptStatus.OK)
                {
                    ed.WriteMessage("\n명령이 취소되었습니다.");
                    return;
                }

                Point3d startPoint = ppr.Value;
                
                // 선택한 점과 가장 가까운 점 찾기
                Point3d? selectedStartPoint = null;
                double minDist = double.MaxValue;
                foreach (Point3d pt in points)
                {
                    double dist = startPoint.DistanceTo(pt);
                    if (dist < minDist)
                    {
                        minDist = dist;
                        selectedStartPoint = pt;
                    }
                }

                if (!selectedStartPoint.HasValue)
                {
                    ed.WriteMessage("\n시작점을 찾을 수 없습니다.");
                    return;
                }

                // 가장 가까운 점부터 연결하는 정렬
                List<Point3d> sortedPts = SortByDistance(points, selectedStartPoint.Value);

                // 3D POLYLINE 생성
                using (Transaction tr = db.TransactionManager.StartTransaction())
                {
                    BlockTable bt = (BlockTable)tr.GetObject(db.BlockTableId, OpenMode.ForRead);
                    BlockTableRecord btr = (BlockTableRecord)tr.GetObject(bt[BlockTableRecord.ModelSpace], OpenMode.ForWrite);

                    // Point3dCollection 생성
                    Point3dCollection pointCollection = new Point3dCollection();
                    foreach (Point3d pt in sortedPts)
                    {
                        pointCollection.Add(pt);
                    }
                    // 폐합: 첫 점을 마지막에 추가
                    pointCollection.Add(sortedPts[0]);

                    // 3D Polyline 생성
                    Polyline3d poly3d = new Polyline3d(Poly3dType.SimplePoly, pointCollection, false);
                    btr.AppendEntity(poly3d);
                    tr.AddNewlyCreatedDBObject(poly3d, true);

                    tr.Commit();
                }

                ed.WriteMessage("\n✅ 3D closed polyline created successfully.");
            }
            catch (System.Exception ex)
            {
                ed.WriteMessage($"\n[오류] {ex.Message}");
            }
        }

        [CommandMethod("3DOFFSET")]
        public void Create3DOffset()
        {
            Document doc = AcadApp.DocumentManager.MdiActiveDocument;
            Editor ed = doc.Editor;
            Database db = doc.Database;

            try
            {
                // 3D POLYLINE 선택
                PromptSelectionOptions pso = new PromptSelectionOptions
                {
                    MessageForAdding = "\n3D 폴리라인을 선택하세요: "
                };
                SelectionFilter filter = new SelectionFilter(new[]
                {
                    new TypedValue((int)DxfCode.Start, "POLYLINE")
                });

                PromptSelectionResult selRes = ed.GetSelection(pso, filter);
                if (selRes.Status != PromptStatus.OK)
                {
                    ed.WriteMessage("\n선택이 취소되었습니다.");
                    return;
                }

                // 높이 입력
                PromptDoubleOptions pdoHeight = new PromptDoubleOptions("\n높이 (+ = 위, - = 아래): ")
                {
                    AllowNegative = true,
                    AllowZero = true
                };
                PromptDoubleResult pdrHeight = ed.GetDouble(pdoHeight);
                if (pdrHeight.Status != PromptStatus.OK)
                {
                    ed.WriteMessage("\n명령이 취소되었습니다.");
                    return;
                }
                double height = pdrHeight.Value;

                // 굴착구배(경사) 입력 (수평거리:수직거리 비율, 예: 1.5 = 1.5:1)
                PromptDoubleOptions pdoSlope = new PromptDoubleOptions("\n굴착구배 (수평거리:수직거리 비율, 예: 1.5 = 1.5:1): ")
                {
                    AllowNegative = false,
                    AllowZero = false,
                    DefaultValue = 1.5
                };
                PromptDoubleResult pdrSlope = ed.GetDouble(pdoSlope);
                if (pdrSlope.Status != PromptStatus.OK)
                {
                    ed.WriteMessage("\n명령이 취소되었습니다.");
                    return;
                }
                double slope = pdrSlope.Value;

                // 수평거리 계산: 높이 × 굴착구배
                // 예: 높이 3, 굴착구배 1.5 → 수평거리 = 3 × 1.5 = 4.5 (수평거리:수직거리 = 1.5:1)
                double offsetDistance = Math.Abs(height) * slope;
                // 높이가 음수면 오프셋 방향도 반대
                if (height < 0)
                    offsetDistance = -offsetDistance;
                
                double zOffset = height;

                using (Transaction tr = db.TransactionManager.StartTransaction())
                {
                    BlockTable bt = (BlockTable)tr.GetObject(db.BlockTableId, OpenMode.ForRead);
                    BlockTableRecord btr = (BlockTableRecord)tr.GetObject(bt[BlockTableRecord.ModelSpace], OpenMode.ForWrite);

                    foreach (SelectedObject selObj in selRes.Value)
                    {
                        if (selObj == null) continue;
                        Polyline3d poly3d = tr.GetObject(selObj.ObjectId, OpenMode.ForRead) as Polyline3d;
                        if (poly3d == null) continue;

                        // 3D 폴리라인인지 확인
                        if (poly3d.PolyType != Poly3dType.SimplePoly) continue;

                        // 점들 추출
                        List<Point3d> points = new List<Point3d>();
                        foreach (ObjectId vertexId in poly3d)
                        {
                            DBObject dbObj = tr.GetObject(vertexId, OpenMode.ForRead);
                            if (dbObj is PolylineVertex3d vertex)
                            {
                                points.Add(vertex.Position);
                            }
                        }

                        if (points.Count < 3)
                        {
                            ed.WriteMessage("\n최소 3개의 점이 필요합니다.");
                            continue;
                        }

                        // 오프셋 계산
                        List<Point3d> offsetPoints = ComputePreciseOffset(points, offsetDistance, zOffset);

                        if (offsetPoints.Count >= 2)
                        {
                            // Point3dCollection 생성
                            Point3dCollection offsetCollection = new Point3dCollection();
                            foreach (Point3d pt in offsetPoints)
                            {
                                offsetCollection.Add(pt);
                            }
                            // 폐합: 첫 점을 마지막에 추가
                            offsetCollection.Add(offsetPoints[0]);

                            // 오프셋된 3D 폴리라인 생성
                            Polyline3d offsetPoly = new Polyline3d(Poly3dType.SimplePoly, offsetCollection, false);
                            btr.AppendEntity(offsetPoly);
                            tr.AddNewlyCreatedDBObject(offsetPoly, true);
                        }
                    }

                    tr.Commit();
                }

                ed.WriteMessage("\n✅ Offset 3D polyline(s) created.");
            }
            catch (System.Exception ex)
            {
                ed.WriteMessage($"\n[오류] {ex.Message}");
            }
        }

        [CommandMethod("3DPOLYOFFSET")]
        public void ShowDialog()
        {
            try
            {
                MainWindow window = new MainWindow();
                window.ShowDialog();
            }
            catch (System.Exception ex)
            {
                Editor ed = AcadApp.DocumentManager.MdiActiveDocument.Editor;
                ed.WriteMessage($"\n[오류] {ex.Message}");
            }
        }

        private List<Point3d> SortByDistance(List<Point3d> points, Point3d startPoint)
        {
            if (points == null || points.Count == 0)
                return new List<Point3d>();

            List<Point3d> result = new List<Point3d>();
            List<Point3d> remaining = new List<Point3d>(points);

            // 시작점 찾기 및 제거
            Point3d current = startPoint;
            if (!remaining.Remove(current))
            {
                // 시작점이 리스트에 없으면 가장 가까운 점 찾기
                double minDist = double.MaxValue;
                Point3d nearest = remaining[0];
                foreach (Point3d pt in remaining)
                {
                    double dist = startPoint.DistanceTo(pt);
                    if (dist < minDist)
                    {
                        minDist = dist;
                        nearest = pt;
                    }
                }
                current = nearest;
                remaining.Remove(current);
            }
            result.Add(current);

            while (remaining.Count > 0)
            {
                Point3d nearest = remaining[0];
                double minDist = current.DistanceTo(nearest);

                foreach (Point3d pt in remaining)
                {
                    double dist = current.DistanceTo(pt);
                    if (dist < minDist)
                    {
                        minDist = dist;
                        nearest = pt;
                    }
                }

                result.Add(nearest);
                remaining.Remove(nearest);
                current = nearest;
            }

            return result;
        }

        private Point2d? ComputeIntersection(Point2d p1, Point2d p2, Point2d p3, Point2d p4)
        {
            double a1 = p2.Y - p1.Y;
            double b1 = p1.X - p2.X;
            double c1 = a1 * p1.X + b1 * p1.Y;

            double a2 = p4.Y - p3.Y;
            double b2 = p3.X - p4.X;
            double c2 = a2 * p3.X + b2 * p3.Y;

            double det = a1 * b2 - a2 * b1;
            if (Math.Abs(det) < 1e-10)
                return null;

            double x = (b2 * c1 - b1 * c2) / det;
            double y = (a1 * c2 - a2 * c1) / det;
            return new Point2d(x, y);
        }

        private List<Point3d> ComputePreciseOffset(List<Point3d> points, double offsetDistance, double zOffset)
        {
            int n = points.Count;
            if (n < 3)
                return new List<Point3d>();

            // 시계 방향 여부 판단 (면적 계산)
            double area = 0;
            for (int i = 0; i < n; i++)
            {
                Point3d p1 = points[i];
                Point3d p2 = points[(i + 1) % n];
                area += (p2.X - p1.X) * (p2.Y + p1.Y);
            }
            bool clockwise = area > 0;

            // 오프셋 방향 정정
            double adjustedOffset = offsetDistance;
            if (offsetDistance > 0)
            {
                adjustedOffset = clockwise ? -offsetDistance : offsetDistance;
            }
            else
            {
                adjustedOffset = clockwise ? offsetDistance : -offsetDistance;
            }

            // Z 오프셋 적용
            List<double> zBaseOffset = new List<double>();
            for (int i = 0; i < n; i++)
            {
                zBaseOffset.Add(points[i].Z + zOffset);
            }

            // 각 선분에 대한 오프셋 벡터 계산
            List<Tuple<Point2d, Point2d, double, double>> offsetLines = new List<Tuple<Point2d, Point2d, double, double>>();
            for (int i = 0; i < n; i++)
            {
                Point3d p1 = points[i];
                Point3d p2 = points[(i + 1) % n];

                Vector2d v = new Vector2d(p2.X - p1.X, p2.Y - p1.Y);
                double length = v.Length;
                if (length < 1e-10)
                    continue;

                // 수직 벡터 (왼쪽으로 90도 회전)
                Vector2d normal = new Vector2d(-v.Y, v.X) / length;
                Vector2d offsetVec = normal * adjustedOffset;

                Point2d q1 = new Point2d(p1.X, p1.Y) + offsetVec;
                Point2d q2 = new Point2d(p2.X, p2.Y) + offsetVec;

                double z1 = zBaseOffset[i];
                double z2 = zBaseOffset[(i + 1) % n];

                offsetLines.Add(new Tuple<Point2d, Point2d, double, double>(q1, q2, z1, z2));
            }

            // 교차점 계산으로 최종 오프셋 점들 생성
            List<Point3d> offsetPoints = new List<Point3d>();
            for (int i = 0; i < offsetLines.Count; i++)
            {
                int prevIdx = (i - 1 + offsetLines.Count) % offsetLines.Count;
                var prevLine = offsetLines[prevIdx];
                var currLine = offsetLines[i];

                Point2d? intersection = ComputeIntersection(
                    prevLine.Item1, prevLine.Item2,
                    currLine.Item1, currLine.Item2);

                if (intersection.HasValue)
                {
                    offsetPoints.Add(new Point3d(intersection.Value.X, intersection.Value.Y, currLine.Item3));
                }
                else
                {
                    offsetPoints.Add(new Point3d(currLine.Item1.X, currLine.Item1.Y, currLine.Item3));
                }
            }

            return offsetPoints;
        }
    }
}

