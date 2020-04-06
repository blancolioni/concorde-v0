with WL.String_Maps;

with Nazar.Views.Gtk_Views.Application;
with Nazar.Views.Gtk_Views.Box;
with Nazar.Views.Gtk_Views.Console;
with Nazar.Views.Gtk_Views.Draw;
with Nazar.Views.Gtk_Views.Label;
with Nazar.Views.Gtk_Views.Layout;
with Nazar.Views.Gtk_Views.Scale;
with Nazar.Views.Gtk_Views.Table;
with Nazar.Views.Orientable;

package body Nazar.Builder.Gtk_Creator is

   type View_Creator is access
     function return Nazar.Views.Nazar_View;

   package View_Creator_Maps is
     new WL.String_Maps (View_Creator);

   Creator_Map : View_Creator_Maps.Map;

   procedure Initialize_Creator_Map;

   type Gtk_Creator_Record is
     new Nazar_Creator_Interface with null record;

   overriding function Create_View
     (Creator : Gtk_Creator_Record;
      Name    : String)
      return Nazar.Views.Nazar_View;

   function Create_Console return Nazar.Views.Nazar_View
   is (Nazar.Views.Gtk_Views.Console.Nazar_Gtk_Console_View_Create);

   function Create_Draw return Nazar.Views.Nazar_View
   is (Nazar.Views.Gtk_Views.Draw.Nazar_Gtk_Draw_View_Create);

   function Create_Horizontal_Box return Nazar.Views.Nazar_View
   is (Nazar.Views.Gtk_Views.Box.Nazar_Gtk_Box_View_Create
       (Nazar.Views.Orientable.Horizontal));

   function Create_Vertical_Box return Nazar.Views.Nazar_View
   is (Nazar.Views.Gtk_Views.Box.Nazar_Gtk_Box_View_Create
       (Nazar.Views.Orientable.Vertical));

   function Create_Grid return Nazar.Views.Nazar_View
   is (Nazar.Views.Gtk_Views.Layout.Nazar_Gtk_Layout_View_Create);

   function Create_Label return Nazar.Views.Nazar_View
   is (Nazar.Views.Gtk_Views.Label.Nazar_Gtk_Label_View_Create);

   function Create_Scale return Nazar.Views.Nazar_View
   is (Nazar.Views.Gtk_Views.Scale.Nazar_Gtk_Scale_View_Create);

   function Create_Table return Nazar.Views.Nazar_View
   is (Nazar.Views.Gtk_Views.Table.Nazar_Gtk_Table_View_Create);

   -----------------
   -- Create_View --
   -----------------

   overriding function Create_View
     (Creator : Gtk_Creator_Record;
      Name    : String)
      return Nazar.Views.Nazar_View
   is
      pragma Unreferenced (Creator);
   begin
      if not Creator_Map.Contains (Name) then
         raise Constraint_Error
           with "gtk-creator: unsupported view type: "
           & "'"  & Name & "'";
      end if;

      return Creator_Map.Element (Name).all;
   end Create_View;

   ---------------------
   -- Get_Gtk_Creator --
   ---------------------

   function Get_Gtk_Creator return Nazar_Creator_Interface'Class is
   begin
      if Creator_Map.Is_Empty then
         Initialize_Creator_Map;
      end if;

      return Gtk_Creator_Record'(null record);
   end Get_Gtk_Creator;

   ----------------------------
   -- Initialize_Creator_Map --
   ----------------------------

   procedure Initialize_Creator_Map is
   begin
      Creator_Map.Insert
        ("application",
         Nazar.Views.Gtk_Views.Application
         .Nazar_Gtk_Application_View_Create'Access);
      Creator_Map.Insert
        ("console", Create_Console'Access);
      Creator_Map.Insert
        ("draw", Create_Draw'Access);
      Creator_Map.Insert
        ("grid", Create_Grid'Access);
      Creator_Map.Insert
        ("horizontal-box", Create_Horizontal_Box'Access);
      Creator_Map.Insert
        ("label", Create_Label'Access);
      Creator_Map.Insert
        ("scale", Create_Scale'Access);
      Creator_Map.Insert
        ("table", Create_Table'Access);
      Creator_Map.Insert
        ("vertical-box", Create_Vertical_Box'Access);
   end Initialize_Creator_Map;

end Nazar.Builder.Gtk_Creator;
