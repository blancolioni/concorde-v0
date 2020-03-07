with Nazar.Interfaces.Table;
with Nazar.Values;

package Nazar.Models.Array_Table is

   type Nazar_Array_Table_Model_Record is
     abstract new Nazar_Model_Record
     and Nazar.Interfaces.Table.Nazar_Table_Interface
   with private;

   function Row_Count
     (Container : Nazar_Array_Table_Model_Record)
      return Natural
      is abstract;

   function Element
     (Container   : Nazar_Array_Table_Model_Record;
      Row, Column : Positive)
      return Nazar.Values.Nazar_Value
      is abstract
     with Pre'Class => Row <= Container.Row_Count
     and then Column <= Container.Column_Count;

private

   type Nazar_Array_Table_Model_Record is
     abstract new Nazar_Model_Record
     and Nazar.Interfaces.Table.Nazar_Table_Interface
   with record
      null;
   end record;

end Nazar.Models.Array_Table;
