with Concorde.Handles.Resource;
with Concorde.Handles.World_Sector;

package Concorde.Sectors is

   function Resource_Yield
     (Sector   : Concorde.Handles.World_Sector.World_Sector_Class;
      Resource : Concorde.Handles.Resource.Resource_Class)
      return Non_Negative_Real;

end Concorde.Sectors;
