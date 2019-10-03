with Ada.Containers.Indefinite_Holders;

package body Concorde.UI is

   package UI_Holders is
     new Ada.Containers.Indefinite_Holders (UI_Interface'Class);

   Holder : UI_Holders.Holder;

   ----------------
   -- Current_UI --
   ----------------

   function Current_UI return UI_Interface'Class is
   begin
      return Holder.Element;
   end Current_UI;

   -------------------
   -- On_UI_Started --
   -------------------

   procedure On_UI_Started (UI : UI_Interface'Class) is
   begin
      Holder := UI_Holders.To_Holder (UI);
   end On_UI_Started;

end Concorde.UI;
