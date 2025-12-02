using System;
using System.Windows;
using Autodesk.AutoCAD.ApplicationServices;
using AcadApp = Autodesk.AutoCAD.ApplicationServices.Application;

namespace _3D_Poly_and_Offset
{
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();
        }

        private void Btn3DPolyLine_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                Document doc = AcadApp.DocumentManager.MdiActiveDocument;
                doc.SendStringToExecute("3DPOLYLINE ", true, false, false);
                this.Close();
            }
            catch (Exception ex)
            {
                MessageBox.Show($"오류: {ex.Message}", "3D PolyLine", MessageBoxButton.OK, MessageBoxImage.Error);
            }
        }

        private void Btn3DOffset_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                Document doc = AcadApp.DocumentManager.MdiActiveDocument;
                doc.SendStringToExecute("3DOFFSET ", true, false, false);
                this.Close();
            }
            catch (Exception ex)
            {
                MessageBox.Show($"오류: {ex.Message}", "3D Offset", MessageBoxButton.OK, MessageBoxImage.Error);
            }
        }
    }
}
