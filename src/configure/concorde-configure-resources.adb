with Ada.Characters.Handling;

with WL.Random;

with Concorde.Elementary_Functions;
with Concorde.Random;
with Concorde.Solar_System;

with Concorde.Db.Deposit;
with Concorde.Db.Gas;
with Concorde.Db.Resource_Sphere;
with Concorde.Db.World_Sector;

package body Concorde.Configure.Resources is

   -------------------------------------
   -- Configure_Atmosphere_Components --
   -------------------------------------

   procedure Configure_Atmosphere_Components
     (Config : Tropos.Configuration)
   is
   begin
      for Cfg of Config loop
         declare
            use Concorde.Solar_System;
            function Get (Name : String)
                          return Non_Negative_Real
            is (Get_Real (Cfg, Name));

            Formula      : constant String :=
                             Cfg.Get ("formula", Cfg.Config_Name);
            Weight       : constant Non_Negative_Real := Get ("weight");
            MP           : constant Non_Negative_Real := Get ("mp");
            BP           : constant Non_Negative_Real := Get ("bp");
            Density      : constant Non_Negative_Real := Get ("density");
            Abund_E      : constant Non_Negative_Real := Get ("abunde");
            Abund_S      : constant Non_Negative_Real := Get ("abunds");
            React        : constant Non_Negative_Real := Get ("react");
            Max_IPP_HG   : constant Non_Negative_Real := Get ("max_ipp_hg");
            Max_IPP_PPM  : constant Non_Negative_Real := Get ("max_ipp_ppm");

         begin
            Concorde.Db.Gas.Create
              (Tag              =>
                 Ada.Characters.Handling.To_Lower (Cfg.Config_Name),
               Formula          => Formula,
               Molecular_Weight => Weight,
               Melting_Point    => MP,
               Boiling_Point    => BP,
               Density          => Density,
               Abundance_E      => Abund_E,
               Abundance_S      => Abund_S,
               Reactivity       => React,
               Max_Ipp          =>
                 (if Max_IPP_HG /= 0.0
                  then Max_IPP_HG * Earth_Surface_Pressure / 760.0
                  else Max_IPP_PPM * Earth_Surface_Pressure / 1.0E6));
         end;
      end loop;
   end Configure_Atmosphere_Components;

   ---------------------
   -- Create_Deposits --
   ---------------------

   procedure Create_Deposits
     (World     : Concorde.Db.World.World_Type;
      Generator : Random_Deposit_Generator)
   is
      use Concorde.Elementary_Functions;
      Gen : Random_Deposit_Generator := Generator;
      Initial_Concentration : constant Unit_Real :=
        (0.4 + Concorde.Random.Unit_Random / 2.0)
        ** (World.Radius
            / Concorde.Solar_System.Earth_Radius);
      Concentration         : Unit_Real := Initial_Concentration;
      Deposit_Count         : Positive;
      package Sector_Vectors is
        new Ada.Containers.Vectors
          (Positive, Concorde.Db.World_Sector_Reference,
           Concorde.Db."=");

      Sector_Refs           : Sector_Vectors.Vector;

   begin

      for World_Sector of
        Concorde.Db.World_Sector.Select_By_World
          (World.Get_World_Reference)
      loop
         Sector_Refs.Append (World_Sector.Get_World_Sector_Reference);
      end loop;

      Deposit_Count :=
        Natural'Max
          (Sector_Refs.Last_Index / 20, 1);

      while Concentration > Initial_Concentration / 20.0 loop
         declare
            Pick   : Non_Negative_Real :=
              Concorde.Random.Unit_Random * Gen.Total_Strength;
            Choice : Positive := 1;
         begin
            while Pick > Gen.Resources (Choice).Strength loop
               Pick := Pick - Gen.Resources (Choice).Strength;
               Choice := Choice + 1;
            end loop;

            for I in 1 .. Deposit_Count loop
               declare
                  Sector_Index : constant Natural :=
                    (if Sector_Refs.Is_Empty
                     then 0
                     else WL.Random.Random_Number
                       (1, Sector_Refs.Last_Index));
                  This_Conc : constant Unit_Real :=
                                   Unit_Clamp
                                     ((Concorde.Random.Normal_Random (0.1)
                                      + 1.0)
                                      * Concentration);
               begin
                  Concorde.Db.Deposit.Create
                    (World         => World.Get_World_Reference,
                     World_Sector  =>
                       (if Sector_Refs.Is_Empty
                        then Concorde.Db.Null_World_Sector_Reference
                        else Sector_Refs.Element (Sector_Index)),
                     Resource      => Gen.Resources (Choice).Reference,
                     Concentration => This_Conc,
                     Difficulty    => 1.0 - This_Conc,
                     Available     =>
                       Concorde.Quantities.To_Quantity
                         (Concorde.Random.About
                              ((2.0e5
                               + Gen.Resources (Choice).Strength * 1.0E6),
                               1.0e5)));
                  if not Sector_Refs.Is_Empty then
                     Sector_Refs (Sector_Index) :=
                       Sector_Refs.Element (Sector_Refs.Last_Index);
                     Sector_Refs.Delete_Last;
                  end if;
               end;
            end loop;

            Gen.Total_Strength :=
              Gen.Total_Strength - Gen.Resources (Choice).Strength;
            exit when Gen.Resources.Is_Empty;
            Gen.Resources (Choice) := Gen.Resources.Last_Element;
            Gen.Resources.Delete_Last;
            Concentration := Concentration
              * (Concorde.Random.Unit_Random / 4.0 + 0.25);
            Deposit_Count := Natural'Max (Deposit_Count - 1, 1);
         end;
      end loop;
   end Create_Deposits;

   ----------------------
   -- Create_Generator --
   ----------------------

   function Create_Generator
     (X, Y, Z : Real)
      return Random_Deposit_Generator
   is
      Gen : Random_Deposit_Generator;
   begin
      for Sphere of Concorde.Db.Resource_Sphere.Scan_By_Top_Record loop
         declare
            use Concorde.Elementary_Functions;
            use type Concorde.Db.Resource_Reference;
            Distance : constant Non_Negative_Real :=
              Sqrt ((X - Sphere.Centre_X) ** 2
                    + (Y - Sphere.Centre_Y) ** 2
                    + (Z - Sphere.Centre_Z) ** 2);
            Strength : constant Non_Negative_Real :=
              (if Distance <= Sphere.Radius
               then Sphere.Strength
               else Sphere.Strength
               / ((Distance - Sphere.Radius + 1.0) ** Sphere.Attenuation));
            Found    : Boolean := False;

         begin
            for Item of Gen.Resources loop
               if Item.Reference = Sphere.Resource then
                  Item.Strength := Item.Strength + Strength;
                  Found := True;
                  exit;
               end if;
            end loop;

            if not Found then
               Gen.Resources.Append ((Sphere.Resource, Strength));
            end if;

            Gen.Total_Strength := Gen.Total_Strength + Strength;
         end;

      end loop;
      return Gen;
   end Create_Generator;

end Concorde.Configure.Resources;
