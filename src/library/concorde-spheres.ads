with Ada.Containers.Vectors;

package Concorde.Spheres is

   type Surface_Point is
      record
         X, Y, Z : Signed_Unit_Real;
      end record;

   package Surface_Point_Vectors is
     new Ada.Containers.Vectors (Positive, Surface_Point);

   procedure Random_Sphere_Points
     (Point_List : in out Surface_Point_Vectors.Vector;
      Count      : Natural);

   procedure Spiral_Sphere_Points
     (Point_List : in out Surface_Point_Vectors.Vector;
      Count      : Natural);

end Concorde.Spheres;
