package Nazar.Interfaces.Text_Writer is

   type Text_Class is (Standard_Text, Error_Text);

   type Text_Writer_Interface is interface;

   procedure Put
     (Writer : in out Text_Writer_Interface;
      Class  : Text_Class;
      Text   : String)
   is abstract;

   procedure New_Line
     (Model : in out Text_Writer_Interface;
      Class : Text_Class)
   is abstract;

   procedure Flush
     (Model : in out Text_Writer_Interface)
   is abstract;

   procedure Put
     (Writer : in out Text_Writer_Interface'Class;
      Text   : String);

   procedure New_Line
     (Writer : in out Text_Writer_Interface'Class);

   procedure Put_Line
     (Writer : in out Text_Writer_Interface'Class;
      Class  : Text_Class;
      Line   : String);

   procedure Put_Line
     (Writer : in out Text_Writer_Interface'Class;
      Line   : String);

   procedure Put_Lines
     (Writer    : in out Text_Writer_Interface'Class;
      Lines     : String;
      Separator : Character := Character'Val (10));

   function Line (Text : String) return String;

private

   function Line (Text : String) return String
   is (Text & Character'Val (10));

end Nazar.Interfaces.Text_Writer;
