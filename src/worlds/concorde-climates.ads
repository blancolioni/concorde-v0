with Concorde.Db;

package Concorde.Climates is

   function Airless return Concorde.Handles.Climate_Reference;
   function Iceball return Concorde.Handles.Climate_Reference;
   function Jovian return Concorde.Handles.Climate_Reference;
   function Martian return Concorde.Handles.Climate_Reference;
   function Temperate return Concorde.Handles.Climate_Reference;
   function Venusian return Concorde.Handles.Climate_Reference;
   function Water return Concorde.Handles.Climate_Reference;

   function Name
     (Climate : Concorde.Handles.Climate_Reference)
      return String;

   function Habitability
     (Climate : Concorde.Handles.Climate_Reference)
      return Unit_Real;

   function Default_Terrain
     (Climate : Concorde.Handles.Climate_Reference)
      return Concorde.Handles.Terrain_Handle;

end Concorde.Climates;
