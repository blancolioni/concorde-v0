with Nazar.Models.Values;

package Nazar.Models.Tables is

   type Table_Model_Interface is interface;

   function Column_Count
     (Table : Table_Model_Interface)
      return Natural
      is abstract;

   function Column_Name
     (Table : Table_Model_Interface;
      Column_Index : Positive)
      return String
      is abstract
     with Pre'Class => Column_Index <= Table.Column_Count;

   function Column_Heading
     (Table        : Table_Model_Interface;
      Column_Index : Positive)
      return String
      is abstract
     with Pre'Class => Column_Index <= Table.Column_Count;

   type Table_Cursor_Interface is interface;

   function Has_Element (Position : Table_Cursor_Interface) return Boolean
                         is abstract;

   procedure Next (Position : in out Table_Cursor_Interface)
   is abstract
     with Pre'Class => Position.Has_Element;

   type Row_Cursor_Interface is interface and Table_Cursor_Interface;

   function First_Row (Table : Table_Model_Interface)
                       return Row_Cursor_Interface'Class
                       is abstract;

   type Cell_Cursor_Interface is interface and Table_Cursor_Interface;

   function First_Cell
     (Row : Row_Cursor_Interface)
      return Cell_Cursor_Interface'Class
      is abstract;

   function Value (Position : Cell_Cursor_Interface)
                   return Nazar.Models.Values.Model_Value_Interface'Class
                   is abstract;

end Nazar.Models.Tables;
