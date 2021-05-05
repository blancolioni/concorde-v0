with Ada.Numerics;
with Concorde.Elementary_Functions;
with Concorde.Logging;
with Concorde.Real_Images;
with Concorde.Handles.Resource_Constraint;
with Concorde.Db;

package body Concorde.Sectors is

   Root_2_Pi : constant Real :=
                 Concorde.Elementary_Functions.Sqrt
                   (1.0 * Ada.Numerics.Pi)
                   with Unreferenced;

   function Image (X : Real) return String
                   renames Concorde.Real_Images.Approximate_Image;

   function Calculate_Yield
     (Value, Min, Max : Real)
      return Unit_Real;

   ---------------------
   -- Calculate_Yield --
   ---------------------

   function Calculate_Yield
     (Value, Min, Max : Real)
      return Unit_Real
   is
      use Concorde.Elementary_Functions;
      Mean  : constant Real := Min + (Max - Min) / 2.0;
      Sigma : constant Real := Sqrt (Mean - Min);

      function Normal (X : Real) return Real
      is (Exp (-1.0 * (X ** 2 / 2.0)));

   begin
      return Normal ((Value - Mean) / Sigma);
   end Calculate_Yield;

   --------------------
   -- Resource_Yield --
   --------------------

   function Resource_Yield
     (Sector   : Concorde.Handles.World_Sector.World_Sector_Class;
      Resource : Concorde.Handles.Resource.Resource_Class)
      return Non_Negative_Real
   is
      use type Concorde.Db.Life_Complexity_Type;
      subtype Resource_Constraint_Class is
        Concorde.Handles.Resource_Constraint.Resource_Constraint_Class;
      Constraint : constant Resource_Constraint_Class :=
                     Concorde.Handles.Resource_Constraint.First_By_Resource
                       (Resource);
      Yield : Unit_Real := 1.0;
   begin
      if Constraint.Life_Constraint then
         if Sector.World.Life < Constraint.Min_Lifeforms then
            Yield := 0.0;
         end if;
      end if;

      if Constraint.Terrain_Constraint then
         if Sector.Terrain.Tag /= Constraint.Terrain.Tag then
            Yield := 0.0;
         end if;
      end if;

      if Constraint.Temperature_Constraint then
         declare
            This : constant Real :=
              Calculate_Yield
                (Sector.Average_Temperature,
                 Constraint.Min_Temperature,
                 Constraint.Max_Temperature);
         begin
            Concorde.Logging.Log
              (Category => Resource.Tag,
               Message  =>
                 "temperature: min "
               & Image (Constraint.Min_Temperature - 273.0)
               & "; max "
               & Image (Constraint.Max_Temperature - 273.0)
               & "; average "
               & Image (Sector.Average_Temperature - 273.0)
               & "; base yield "
               & Image (This * 100.0) & "%"
               & "; final yield "
               & Image (This * Resource.Yield * 100.0) & "%");

            Yield := Real'Min (Yield, This * Resource.Yield);
         end;
      end if;

      if Constraint.Moisture_Constraint then
         declare
            This : constant Real :=
              Calculate_Yield
                (Sector.Moisture,
                 Constraint.Min_Moisture,
                 Constraint.Max_Moisture);
         begin
            Concorde.Logging.Log
              (Category => Resource.Tag,
               Message  =>
                 "moisture: min "
               & Image (Constraint.Min_Moisture * 100.0) & "%"
               & "; max "
               & Image (Constraint.Max_Moisture * 100.0) & "%"
               & "; average "
               & Image (Sector.Moisture * 100.0) & "%"
               & "; yield "
               & Image (This * 100.0) & "%");

            Yield := Real'Min (Yield, This);
         end;
      end if;

      return Yield;
   end Resource_Yield;

end Concorde.Sectors;
