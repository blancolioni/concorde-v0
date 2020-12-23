with Concorde.Handles.World_Sector;

package body Concorde.Sectors is

   -------------------------
   -- Has_Stock_Reference --
   -------------------------

   function Has_Stock_Reference
     (Sector : Concorde.Handles.World_Sector_Reference)
      return Concorde.Handles.Has_Stock_Reference
   is
   begin
      return Concorde.Handles.World_Sector.Get (Sector).Get_Has_Stock_Reference;
   end Has_Stock_Reference;

end Concorde.Sectors;
