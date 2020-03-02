with Ada.Text_IO;

package body Concorde.UI.Web_UI is

   type Web_UI_Type is
     new UI_Interface with
      record
         null;
      end record;

   overriding procedure Start
     (Web_UI  : in out Web_UI_Type);

   overriding procedure Stop
     (Item    : in out Web_UI_Type;
      Message : String);

   Local_Web_UI : aliased Web_UI_Type;

   ----------------
   -- Get_Web_UI --
   ----------------

   function Get_Web_UI return UI_Type is
   begin
      return Local_Web_UI'Access;
   end Get_Web_UI;

   -----------
   -- Start --
   -----------

   overriding procedure Start (Web_UI  : in out Web_UI_Type) is
      pragma Unreferenced (Web_UI);
   begin

      null;

   end Start;

   ----------
   -- Stop --
   ----------

   overriding procedure Stop
     (Item    : in out Web_UI_Type;
      Message : String)
   is
      pragma Unreferenced (Item);
   begin
      Ada.Text_IO.Put_Line (Message);
   end Stop;

end Concorde.UI.Web_UI;
