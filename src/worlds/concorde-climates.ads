with Concorde.Db;

package Concorde.Climates is

   function Airless return Concorde.Db.Climate_Reference;
   function Iceball return Concorde.Db.Climate_Reference;
   function Jovian return Concorde.Db.Climate_Reference;
   function Martian return Concorde.Db.Climate_Reference;
   function Temperate return Concorde.Db.Climate_Reference;
   function Venusian return Concorde.Db.Climate_Reference;
   function Water return Concorde.Db.Climate_Reference;

   function Name
     (Climate : Concorde.Db.Climate_Reference)
      return String;

   function Habitability
     (Climate : Concorde.Db.Climate_Reference)
      return Unit_Real;

   function Default_Terrain
     (Climate : Concorde.Db.Climate_Reference)
      return Concorde.Db.Terrain_Reference;

end Concorde.Climates;
