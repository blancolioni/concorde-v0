package Nazar.Models.Tables.Array_Tables is

   type Array_Table_Interface is interface;

   function Row_Count
     (Container : Array_Table_Interface)
      return Natural
      is abstract;

   function Column_Count
     (Container : Array_Table_Interface)
      return Natural
      is abstract;

   function Element
     (Container   : Array_Table_Interface;
      Row, Column : Positive)
      return Nazar.Models.Values.Model_Value_Interface'Class
      is abstract
     with Pre'Class => Row <= Container.Row_Count
     and then Column <= Container.Column_Count;

   function To_Table_Model
     (Container : Array_Table_Interface'Class)
     return Table_Model_Interface'Class;

end Nazar.Models.Tables.Array_Tables;
