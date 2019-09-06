with Concorde.Db;

package Concorde.Properties is

   subtype Property_Type is Concorde.Db.Property_Reference;

   function Property
     (Name : String)
      return Property_Type;

end Concorde.Properties;
