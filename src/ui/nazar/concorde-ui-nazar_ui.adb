with Ada.Text_IO;

with Nazar.Views;

with Concorde.Paths;

package body Concorde.UI.Nazar_UI is

   type Root_Nazar_UI is
     new UI_Interface with
      record
         Top : Nazar.Views.Nazar_View;
      end record;

   overriding procedure Start (UI : in out Root_Nazar_UI);

   overriding procedure Stop (UI : in out Root_Nazar_UI;
                              Message : String);

   type Nazar_UI_Access is access all Root_Nazar_UI'Class;

   ------------------
   -- Get_Nazar_UI --
   ------------------

   function Get_Nazar_UI
     (Creator : Nazar.Builder.Nazar_Creator_Interface'Class) return UI_Type
   is
      Result : constant Nazar_UI_Access := new Root_Nazar_UI;
      Builder : constant Nazar.Builder.Nazar_Builder :=
        Nazar.Builder.Nazar_Builder_New
          (Creator     => Creator,
           Config_Path => Concorde.Paths.Config_File ("ui/concorde.config"));
   begin
      Result.Top := Builder.Get_View ("Concorde");
      return UI_Type (Result);
   end Get_Nazar_UI;

   -----------
   -- Start --
   -----------

   overriding procedure Start (UI : in out Root_Nazar_UI) is
   begin
      UI.Top.Show;
   end Start;

   ----------
   -- Stop --
   ----------

   overriding procedure Stop
     (UI      : in out Root_Nazar_UI;
      Message : String)
   is
   begin
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error, Message);
      UI.Top.Destroy;
   end Stop;

end Concorde.UI.Nazar_UI;
