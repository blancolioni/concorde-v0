with WL.String_Maps;

with Concorde.Db.Deposit;

with Concorde.Handles.World_Sector;

package body Concorde.Colonies.Sectors is

   type Sector_Score_Function is
     access function (Sector : Concorde.Db.World_Sector_Reference) return Real;

   package Sector_Score_Maps is
     new WL.String_Maps (Sector_Score_Function);

   Score_Fn : Sector_Score_Maps.Map;

   procedure Check_Map;

   function Score_Farmland
     (Sector : Concorde.Db.World_Sector_Reference)
      return Real;

   function Score_Mine
     (Sector : Concorde.Db.World_Sector_Reference)
      return Real;

   function Score_Urban
     (Sector : Concorde.Db.World_Sector_Reference)
      return Real
   is (Score_Farmland (Sector) + 1.0);

   ---------------
   -- Check_Map --
   ---------------

   procedure Check_Map is
   begin
      if Score_Fn.Is_Empty then
         Score_Fn.Insert ("farmland", Score_Farmland'Access);
         Score_Fn.Insert ("mine", Score_Mine'Access);
         Score_Fn.Insert ("urban", Score_Urban'Access);
      end if;
   end Check_Map;

   --------------------
   -- Score_Farmland --
   --------------------

   function Score_Farmland
     (Sector : Concorde.Db.World_Sector_Reference)
      return Real
   is
      Handle : constant Concorde.Handles.World_Sector.World_Sector_Handle :=
                 Concorde.Handles.World_Sector.Get (Sector);
   begin
      if Handle.Terrain.Is_Water then
         return Real'First;
      end if;
      if Handle.Average_Temperature not in 273.0 .. 303.0 then
         return 0.0;
      end if;
      if Handle.Average_Temperature < 293.0 then
         return Handle.Average_Temperature - 273.0;
      else
         return 303.0 - Handle.Average_Temperature;
      end if;
   end Score_Farmland;

   ----------------
   -- Score_Mine --
   ----------------

   function Score_Mine
     (Sector : Concorde.Db.World_Sector_Reference)
      return Real
   is
   begin
      return Result : Real := 0.0 do
         for Deposit of
           Concorde.Db.Deposit.Select_By_World_Sector (Sector)
         loop
            Result := Result + Deposit.Concentration;
         end loop;
      end return;
   end Score_Mine;

   ------------------
   -- Score_Sector --
   ------------------

   function Score_Sector
     (Sector         : Concorde.Db.World_Sector_Reference;
      Sector_Use_Tag : String)
      return Real
   is
   begin
      Check_Map;
      if not Score_Fn.Contains (Sector_Use_Tag) then
         return 0.0;
      else
         return Score_Fn.Element (Sector_Use_Tag) (Sector);
      end if;
   end Score_Sector;

end Concorde.Colonies.Sectors;
