with Ada.Characters.Handling;

with Concorde.Climates;
with Concorde.Solar_System;

with Concorde.Db.World;

package body Concorde.UI.Models.World is

   ------------
   -- Create --
   ------------

   function Create
     (World : Concorde.Db.World_Reference)
      return World_Model
   is
      function Nice (Image : String) return String;

      function Category_Name
        (Category : Concorde.Db.World_Category)
         return String
      is (Nice (Category'Image));

      ----------
      -- Nice --
      ----------

      function Nice (Image : String) return String is
         use Ada.Characters.Handling;
         First : Boolean := True;
      begin
         return Result : String := Image do
            for Ch of Result loop
               if First then
                  Ch := To_Upper (Ch);
                  First := False;
               elsif Is_Letter (Ch) then
                  Ch := To_Lower (Ch);
               elsif Ch in '-' | '_' then
                  Ch := ' ';
                  First := True;
               end if;
            end loop;
         end return;
      end Nice;

   begin
      return Model : constant World_Model :=
        new Root_World_Model
      do

         Model.Reference := World;

         Model.Add_Column ("name");
         Model.Add_Column ("category");
         Model.Add_Column ("climate");
         Model.Add_Column ("earth-mass");
         Model.Add_Column ("earth-radius");
         Model.Add_Column ("g");
         Model.Add_Column ("temperature");
         Model.Add_Column ("habitability");
         Model.Add_Column ("water-coverage");
         Model.Add_Column ("ice-coverage");
         Model.Add_Column ("day-length");

         declare
            use Concorde.Solar_System;
            Row   : constant Concorde.UI.Models.Tables.Table_Row_Index :=
                      Model.Add_Row;
            Rec   : constant Concorde.Db.World.World_Type :=
                      Concorde.Db.World.Get (World);
         begin
            Model.Set_Cell (Row, 1, Rec.Name);
            Model.Set_Cell (Row, 2, Category_Name (Rec.Category));
            Model.Set_Cell (Row, 3, Concorde.Climates.Name (Rec.Climate));
            Model.Set_Cell (Row, 4, Rec.Mass / Earth_Mass);
            Model.Set_Cell (Row, 5, Rec.Radius / Earth_Radius);
            Model.Set_Cell (Row, 6, Rec.Surface_Gravity);
            Model.Set_Cell (Row, 7, Rec.Surface_Temperature - 273.0);
            Model.Set_Cell (Row, 8, Rec.Habitability);
            Model.Set_Cell (Row, 9, Rec.Water_Coverage);
            Model.Set_Cell (Row, 10, Rec.Ice_Coverage);
            Model.Set_Cell (Row, 11, Rec.Rotation_Period / 3600.0);
         end;

         declare
            procedure Add_Sector
              (Sector : Concorde.Db.World_Sector_Reference);

            ----------------
            -- Add_Sector --
            ----------------

            procedure Add_Sector
              (Sector : Concorde.Db.World_Sector_Reference)
            is
               Tiles : constant Concorde.Worlds.Sector_Vertex_Array :=
                         Concorde.Worlds.Get_Vertices (Sector);
               Rec   : constant World_Sector_Type :=
                         World_Sector_Type'
                           (Reference => Sector,
                            Owner     => Concorde.Worlds.Get_Owner (Sector),
                            Boundary  =>
                              Sector_Boundary_Holders.To_Holder (Tiles),
                            Centre    =>
                              Concorde.Worlds.Get_Centre (Sector));
            begin
               Model.Sectors.Append (Rec);
            end Add_Sector;

         begin
            Concorde.Worlds.Scan_Surface
              (World, Add_Sector'Access);
         end;

      end return;
   end Create;

   ------------------
   -- Scan_Surface --
   ------------------

   procedure Scan_Surface
     (Model   : Root_World_Model'Class;
      Process : not null access
        procedure (Sector   : Concorde.Db.World_Sector_Reference;
                   Centre_X : Concorde.Worlds.Sector_Vertex;
                   Boundary : Concorde.Worlds.Sector_Vertex_Array))
   is
   begin
      for Item of Model.Sectors loop
         Process (Item.Reference, Item.Centre, Item.Boundary.Element);
      end loop;
   end Scan_Surface;

end Concorde.UI.Models.World;
