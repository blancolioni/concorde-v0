with Nazar.Models.Text;

package Concorde.UI.Models.Current_Date is

   type Current_Date_Model_Record is
     new Nazar.Models.Text.Nazar_Text_Model_Record
   with private;

   type Current_Date_Model is access all Current_Date_Model_Record'Class;

   function Current_Date_Model_New return Current_Date_Model;

private

   type Current_Date_Model_Record is
     new Nazar.Models.Text.Nazar_Text_Model_Record with
      record
         null;
      end record;

end Concorde.UI.Models.Current_Date;
