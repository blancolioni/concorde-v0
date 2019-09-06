with Concorde.Color;

with Concorde.Db;

package Concorde.Stars is

   type Star_Type is tagged private;

   function Get
     (Reference : Concorde.Db.Star_Reference)
      return Star_Type'Class;

   function Color
     (Star : Star_Type'Class)
      return Concorde.Color.Concorde_Color;

   function Name (Star : Concorde.Db.Star_Reference) return String;
   function Mass (Star : Concorde.Db.Star_Reference) return Non_Negative_Real;
   function Solar_Masses
     (Star : Concorde.Db.Star_Reference)
      return Non_Negative_Real;

   function Spectral_Type
     (Star : Concorde.Db.Star_Reference)
      return String;

private

   type Star_Type is tagged
      record
         Reference : Concorde.Db.Star_Reference;
      end record;

end Concorde.Stars;
