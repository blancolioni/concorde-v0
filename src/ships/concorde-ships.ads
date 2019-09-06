with Concorde.Db.Ship;

package Concorde.Ships is

   type Ship_Type is tagged private;

   function Get (Reference : Concorde.Db.Ship_Reference) return Ship_Type;
   function Get (Ship : Concorde.Db.Ship.Ship_Type) return Ship_Type;

   function Name (Ship : Ship_Type'Class) return String;

   function Owner
     (Ship : Ship_Type'Class)
      return Concorde.Db.Faction_Reference;

   function Star_System
     (Ship : Ship_Type'Class)
      return Concorde.Db.Star_System_Reference;

   function World
     (Ship : Ship_Type'Class)
      return Concorde.Db.World_Reference;

   function Orbit
     (Ship : Ship_Type'Class)
      return Non_Negative_Real;

   function Inclination
     (Ship : Ship_Type'Class)
      return Real;

   function Current_Longitude
     (Ship : Ship_Type'Class)
      return Non_Negative_Real;

   function Dry_Mass
     (Ship : Ship_Type'Class)
      return Non_Negative_Real;

   function Current_Mass
     (Ship : Ship_Type'Class)
      return Non_Negative_Real;

   function Total_Thrust
     (Ship : Ship_Type'Class)
      return Non_Negative_Real;

   function Design_Cargo_Volume
     (Design : Concorde.Db.Ship_Design_Reference)
      return Non_Negative_Real;

   function Design_Delta_V
     (Design     : Concorde.Db.Ship_Design_Reference;
      Cargo_Mass : Non_Negative_Real)
      return Non_Negative_Real;

   function Design_Mass
     (Design : Concorde.Db.Ship_Design_Reference)
      return Non_Negative_Real;

   function Design_Fuel_Mass
     (Design : Concorde.Db.Ship_Design_Reference)
      return Non_Negative_Real;

   function Design_Thrust
     (Design : Concorde.Db.Ship_Design_Reference)
      return Non_Negative_Real;

   procedure Create_Ship
     (Owner   : Concorde.Db.Faction_Reference;
      World   : Concorde.Db.World_Reference;
      Design  : Concorde.Db.Ship_Design_Reference;
      Manager : String;
      Name    : String);

private

   type Ship_Type is tagged
      record
         Reference : Concorde.Db.Ship_Reference;
      end record;

   function Get (Reference : Concorde.Db.Ship_Reference) return Ship_Type
   is (Ship_Type'(Reference => Reference));

   function Get (Ship : Concorde.Db.Ship.Ship_Type) return Ship_Type
   is (Ship_Type'(Reference => Ship.Get_Ship_Reference));

   function Name (Ship : Ship_Type'Class) return String
   is (Concorde.Db.Ship.Get (Ship.Reference).Name);

   function Owner
     (Ship : Ship_Type'Class)
      return Concorde.Db.Faction_Reference
   is (Concorde.Db.Ship.Get (Ship.Reference).Faction);

   function Star_System
     (Ship : Ship_Type'Class)
      return Concorde.Db.Star_System_Reference
   is (Concorde.Db.Ship.Get (Ship.Reference).Star_System);

   function World
     (Ship : Ship_Type'Class)
      return Concorde.Db.World_Reference
   is (Concorde.Db.Ship.Get (Ship.Reference).World);

   function Orbit
     (Ship : Ship_Type'Class)
      return Non_Negative_Real
   is (Concorde.Db.Ship.Get (Ship.Reference).Orbit);

   function Inclination
     (Ship : Ship_Type'Class)
      return Real
   is (Concorde.Db.Ship.Get (Ship.Reference).Inclination);

end Concorde.Ships;
