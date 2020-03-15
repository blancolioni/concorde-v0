with Nazar.Values;

package body Nazar.Views.Gtk_Views.Label is

   procedure On_Text_Property_Update
     (Properties : in out Nazar.Interfaces.Properties
      .Property_Container_Interface'Class;
      New_Value  : Nazar.Values.Nazar_Value);

   ------------------------
   -- Declare_Properties --
   ------------------------

   overriding procedure Declare_Properties
     (View : in out Nazar_Gtk_Label_View_Record)
   is
   begin
      Nazar_Gtk_View_Record (View).Declare_Properties;
      View.Declare_Property
        ("text", "", On_Text_Property_Update'Access);
   end Declare_Properties;

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

   -----------------------------
   -- On_Text_Property_Update --
   -----------------------------

   procedure On_Text_Property_Update
     (Properties : in out Nazar.Interfaces.Properties
      .Property_Container_Interface'Class;
      New_Value  : Nazar.Values.Nazar_Value)
   is
      View : Nazar_Gtk_Label_View_Record renames
               Nazar_Gtk_Label_View_Record (Properties);
      New_Label : constant String :=
                    Nazar.Values.To_String (New_Value);
   begin
      View.Text_Model.Set_Text (New_Label);
   end On_Text_Property_Update;

   -----------------------
   -- Update_From_Model --
   -----------------------

   overriding procedure Update_From_Model
     (View : in out Nazar_Gtk_Label_View_Record)
   is
   begin
      View.Label.Set_Label (View.Text_Model.Get_Text);
   end Update_From_Model;

end Nazar.Views.Gtk_Views.Label;
