with Nazar.Models.Text;

package Concorde.UI.Models is

   type Regular_Update_Interface is interface;

   procedure Refresh
     (Model    : in out Regular_Update_Interface;
      Continue : out Boolean)
   is abstract;

   function Interval
     (Model : Regular_Update_Interface)
      return Duration
      is abstract;

   procedure Start_Updates
     (Model    : not null access Regular_Update_Interface'Class);

   type Dynamic_Text_Model is
     abstract new Nazar.Models.Text.Nazar_Text_Model_Record
     and Regular_Update_Interface
   with private;

   overriding procedure Refresh
     (Model    : in out Dynamic_Text_Model;
      Continue : out Boolean);

   overriding function Interval
     (Model : Dynamic_Text_Model)
      return Duration;

   procedure Initialize
     (Model        : not null access Dynamic_Text_Model'Class;
      Initial_Text : String;
      Interval     : Duration);

   function Current_Text
     (Model : Dynamic_Text_Model)
      return String
      is abstract;

private

   type Dynamic_Text_Model is
     abstract new Nazar.Models.Text.Nazar_Text_Model_Record
     and Regular_Update_Interface with
      record
         Interval : Duration;
      end record;

   overriding function Interval
     (Model : Dynamic_Text_Model)
      return Duration
   is (Model.Interval);

end Concorde.UI.Models;
