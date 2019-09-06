with Concorde.Db.World_Sector;

package body Concorde.Sectors is

   -------------------------
   -- Has_Stock_Reference --
   -------------------------

   function Has_Stock_Reference
     (Sector : Concorde.Db.World_Sector_Reference)
      return Concorde.Db.Has_Stock_Reference
   is
   begin
      return Concorde.Db.World_Sector.Get (Sector).Get_Has_Stock_Reference;
   end Has_Stock_Reference;

end Concorde.Sectors;
