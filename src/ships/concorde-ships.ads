with Concorde.Handles.Ship;

package Concorde.Ships is

   type Ship_Type is tagged private;

   function Get (Reference : Concorde.Handles.Ship_Reference) return Ship_Type;
   function Get (Ship : Concorde.Handles.Ship.Ship_Type) return Ship_Type;

   function Name (Ship : Ship_Type'Class) return String;

   function Owner
     (Ship : Ship_Type'Class)
      return Concorde.Handles.Faction.Faction_Handle;

   function Star_System
     (Ship : Ship_Type'Class)
      return Concorde.Handles.Star_System_Reference;

   function World
     (Ship : Ship_Type'Class)
      return Concorde.Handles.World_Reference;

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
     (Design : Concorde.Handles.Ship_Design_Reference)
      return Non_Negative_Real;

   function Design_Delta_V
     (Design     : Concorde.Handles.Ship_Design_Reference;
      Cargo_Mass : Non_Negative_Real)
      return Non_Negative_Real;

   function Design_Mass
     (Design : Concorde.Handles.Ship_Design_Reference)
      return Non_Negative_Real;

   function Design_Fuel_Mass
     (Design : Concorde.Handles.Ship_Design_Reference)
      return Non_Negative_Real;

   function Design_Thrust
     (Design : Concorde.Handles.Ship_Design_Reference)
      return Non_Negative_Real;

   procedure Create_Ship
     (Owner   : Concorde.Handles.Faction.Faction_Handle;
      World   : Concorde.Handles.World_Reference;
      Design  : Concorde.Handles.Ship_Design_Reference;
      Manager : String;
      Name    : String);

private

   type Ship_Type is tagged
      record
         Reference : Concorde.Handles.Ship_Reference;
      end record;

   function Get (Reference : Concorde.Handles.Ship_Reference) return Ship_Type
   is (Ship_Type'(Reference => Reference));

   function Get (Ship : Concorde.Handles.Ship.Ship_Type) return Ship_Type
   is (Ship_Type'(Reference => Ship.Get_Ship_Reference));

   function Name (Ship : Ship_Type'Class) return String
   is (Concorde.Handles.Ship.Get (Ship.Reference).Name);

   function Owner
     (Ship : Ship_Type'Class)
      return Concorde.Handles.Faction.Faction_Handle
   is (Concorde.Handles.Ship.Get (Ship.Reference).Faction);

   function Star_System
     (Ship : Ship_Type'Class)
      return Concorde.Handles.Star_System_Reference
   is (Concorde.Handles.Ship.Get (Ship.Reference).Star_System);

   function World
     (Ship : Ship_Type'Class)
      return Concorde.Handles.World_Reference
   is (Concorde.Handles.Ship.Get (Ship.Reference).World);

   function Orbit
     (Ship : Ship_Type'Class)
      return Non_Negative_Real
   is (Concorde.Handles.Ship.Get (Ship.Reference).Orbit);

   function Inclination
     (Ship : Ship_Type'Class)
      return Real
   is (Concorde.Handles.Ship.Get (Ship.Reference).Inclination);

end Concorde.Ships;
