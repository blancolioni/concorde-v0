package body Nazar.Views.Gtk_Views.Label is

   ------------------------
   -- Declare_Properties --
   ------------------------

   overriding procedure Declare_Properties
     (View : in out Nazar_Gtk_Label_View_Record)
   is
   begin
      Root_Gtk_View_Type (View).Declare_Properties;
      View.Declare_Property ("text", "");
   end Declare_Properties;

   -------------------
   -- Model_Changed --
   -------------------

   overriding procedure Model_Changed
     (View : in out Nazar_Gtk_Label_View_Record)
   is
   begin
      View.Label.Set_Label (View.Text_Model.Get_Text);
   end Model_Changed;

   ---------------------------------
   -- Nazar_Gtk_Label_View_Create --
   ---------------------------------

   function Nazar_Gtk_Label_View_Create return Nazar_View is
      View : constant Nazar_Gtk_Label_View :=
        Nazar_Gtk_Label_View_New
          (Nazar.Models.Text.Nazar_Text_Model_New (""));
   begin
      return Nazar_View (View);
   end Nazar_Gtk_Label_View_Create;

   ------------------------------
   -- Nazar_Gtk_Label_View_New --
   ------------------------------

   function Nazar_Gtk_Label_View_New
     (Model : not null access Nazar.Models.Text.Nazar_Text_Model_Record'Class)
      return Nazar_Gtk_Label_View
   is
      View : constant Nazar_Gtk_Label_View :=
        new Nazar_Gtk_Label_View_Record;
   begin
      View.Label :=
        Gtk.Label.Gtk_Label_New ("");
      View.Initialize (View.Label);
      View.Set_Model (Model);
      return View;
   end Nazar_Gtk_Label_View_New;

   ------------------------------
   -- Nazar_Gtk_Label_View_New --
   ------------------------------

   function Nazar_Gtk_Label_View_New
     (Text : String)
      return Nazar_Gtk_Label_View
   is
   begin
      return Nazar_Gtk_Label_View_New
        (Nazar.Models.Text.Nazar_Text_Model_New
           (Text));
   end Nazar_Gtk_Label_View_New;

   ------------------
   -- Set_Property --
   ------------------

   overriding procedure Set_Property
     (View           : in out Nazar_Gtk_Label_View_Record;
      Property_Name  : String;
      Property_Value : Nazar.Values.Nazar_Value)
   is
   begin
      Root_Gtk_View_Type (View).Set_Property (Property_Name, Property_Value);
      if Property_Name = "text" then
         declare
            New_Label : constant String :=
              Nazar.Values.To_String (Property_Value);
         begin
            View.Text_Model.Set_Text (New_Label);
         end;
      end if;
   end Set_Property;

end Nazar.Views.Gtk_Views.Label;
