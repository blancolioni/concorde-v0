package body Concorde.Terrain is

   -----------
   -- Ocean --
   -----------

   function Ocean
     return Concorde.Handles.Terrain.Terrain_Handle
   is
   begin
      return Concorde.Handles.Terrain.Get_By_Tag ("ocean");
   end Ocean;

end Concorde.Terrain;
