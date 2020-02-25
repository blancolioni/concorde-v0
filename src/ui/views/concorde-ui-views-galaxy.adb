with Ada.Containers.Doubly_Linked_Lists;

with Concorde.Db.Star;

with Concorde.Handles.Star;
with Concorde.Handles.Star_System.Selections;

package body Concorde.UI.Views.Galaxy is

   type System_Record is
      record
         Handle  : Concorde.Handles.Star_System.Star_System_Handle;
         Color   : Concorde.Color.Concorde_Color;
         X, Y, Z : Real;
      end record;

   package System_Record_Lists is
     new Ada.Containers.Doubly_Linked_Lists (System_Record);

   type Root_Galaxy_View is
     new View_Object_Interface with
      record
         Port    : View_Port;
         Systems : System_Record_Lists.List;
      end record;

   overriding function Display_Title
     (View : Root_Galaxy_View)
      return String
   is ("Galaxy");

   overriding function Display_Area
     (View : Root_Galaxy_View)
      return View_Port
   is (View.Port);

   overriding function Background_Color
     (View : Root_Galaxy_View)
      return Concorde.Color.Concorde_Color
   is (Concorde.Color.Black);

   overriding function Has_Changed
     (View : Root_Galaxy_View)
      return Boolean
   is (False);

   overriding procedure Clear_Changed
     (View : in out Root_Galaxy_View)
   is null;

   overriding procedure Get_Draw_Commands
     (View     : Root_Galaxy_View;
      Factory  : Draw_Command_Factory'Class;
      Commands : in out Draw_Command_List'Class);

   overriding procedure Iterate_Children
     (View     : Root_Galaxy_View;
      Process  : not null access
        procedure (Child : View_Object_Interface'Class))
   is null;

   overriding procedure Iterate_Properties
     (View     : Root_Galaxy_View;
      Process : not null access
        procedure (Property_Name : String;
                   Property_Value : Concorde.UI.Values.Value_Type))
   is null;

   -----------------
   -- Galaxy_View --
   -----------------

   function Galaxy_View return View_Object_Interface'Class is
      Left, Top     : Real := Real'Last;
      Right, Bottom : Real := Real'First;
   begin
      return View : Root_Galaxy_View do
         for Star_System of
           Concorde.Handles.Star_System.Selections.Select_All
         loop
            declare
               Star  : constant Concorde.Handles.Star.Star_Handle :=
                 Concorde.Handles.Star.Get
                   (Concorde.Db.Star.First_Reference_By_Star_System
                      (Star_System.Reference));
               Color : constant Concorde.Color.Concorde_Color :=
                 (Star.Red,
                  Star.Green,
                  Star.Blue,
                  1.0);
               Rec : constant System_Record := System_Record'
                 (Handle => Star_System,
                  Color  => Color,
                  X      => Star_System.X,
                  Y      => Star_System.Y,
                  Z      => Star_System.Z);
            begin
               View.Systems.Append (Rec);
               Left := Real'Min (Left, Star_System.X);
               Top  := Real'Min (Top, Star_System.Y);
               Right := Real'Max (Right, Star_System.X);
               Bottom  := Real'Max (Bottom, Star_System.Y);
            end;
         end loop;
         View.Port := (Left, Top, Right - Left, Bottom - Top);
      end return;
   end Galaxy_View;

   -----------------------
   -- Get_Draw_Commands --
   -----------------------

   overriding procedure Get_Draw_Commands
     (View     : Root_Galaxy_View;
      Factory  : Draw_Command_Factory'Class;
      Commands : in out Draw_Command_List'Class)
   is
   begin
      for Rec of View.Systems loop
         Commands.Add
           (Factory.Move_To (Rec.X, Rec.Y));
         Commands.Add
           (Factory.Color (Rec.Color));
         Commands.Add
           (Factory.Arc (Absolute (2.0)));
         Commands.Add
           (Factory.Render (Fill => True));
      end loop;
   end Get_Draw_Commands;

end Concorde.UI.Views.Galaxy;
