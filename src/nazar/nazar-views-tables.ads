package Nazar.Views.Tables is

   type Table_View_Interface is interface and Nazar_View_Interface;

   type Nazar_Table_View is access all Table_View_Interface'Class;

end Nazar.Views.Tables;
