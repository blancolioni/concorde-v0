with  Nazar.Web_UI.Views;

package Nazar.Web_UI.Main is

   procedure Start
     (Application_Name : String;
      Port             : Natural;
      Top_View         : not null access
        Nazar.Web_UI.Views.Root_AWS_View_Type'Class);

   procedure Stop
     (Message : String);

end Nazar.Web_UI.Main;
