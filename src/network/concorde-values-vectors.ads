with Ada.Containers.Indefinite_Vectors;

package Concorde.Values.Vectors is
  new Ada.Containers.Indefinite_Vectors (Positive, Value_Interface'Class);
