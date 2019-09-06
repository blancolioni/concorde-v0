with Concorde.Solar_System;

with Concorde.Db.Star;

package body Concorde.Stars is

   -----------
   -- Color --
   -----------

   function Color
     (Star : Star_Type'Class)
      return Concorde.Color.Concorde_Color
   is
      Rec : constant Concorde.Db.Star.Star_Type :=
              Concorde.Db.Star.Get (Star.Reference);
   begin
      return (Rec.Red, Rec.Green, Rec.Blue, 1.0);
   end Color;

   function Get
     (Reference : Concorde.Db.Star_Reference)
      return Star_Type'Class
   is
   begin
      return Star_Type'(Reference => Reference);
   end Get;

   ----------
   -- Mass --
   ----------

   function Mass
     (Star : Concorde.Db.Star_Reference)
      return Non_Negative_Real
   is
   begin
      return Concorde.Db.Star.Get (Star).Mass;
   end Mass;

   ----------
   -- Name --
   ----------

   function Name (Star : Concorde.Db.Star_Reference) return String is
   begin
      return Concorde.Db.Star.Get (Star).Name;
   end Name;

   ------------------
   -- Solar_Masses --
   ------------------

   function Solar_Masses
     (Star : Concorde.Db.Star_Reference)
      return Non_Negative_Real
   is
   begin
      return Mass (Star) / Concorde.Solar_System.Solar_Mass;
   end Solar_Masses;

   -------------------
   -- Spectral_Type --
   -------------------

   function Spectral_Type
     (Star : Concorde.Db.Star_Reference)
      return String
   is
      Rec : constant Concorde.Db.Star.Star_Type :=
              Concorde.Db.Star.Get (Star);
   begin
      return Concorde.Db.Spectral_Class'Image (Rec.Class)
        & Character'Val (Rec.Subclass + 48);
   end Spectral_Type;

end Concorde.Stars;
