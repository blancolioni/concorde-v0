with Nazar.Views;
with Nazar.Web_UI.Routes;

package Nazar.Web_UI.Views is

   type Root_AWS_View_Type is
     abstract new Nazar.Views.Root_View_Type
     and Nazar.Web_UI.Routes.Request_Handler_Interface
   with private;

   procedure Connect
     (View : in out Root_AWS_View_Type)
   is null;

   type Nazar_AWS_View is access all Root_AWS_View_Type'Class;

private

   type Root_AWS_View_Type is
     abstract new Nazar.Views.Root_View_Type
     and Nazar.Web_UI.Routes.Request_Handler_Interface with
      record
         null;
      end record;

end Nazar.Web_UI.Views;
