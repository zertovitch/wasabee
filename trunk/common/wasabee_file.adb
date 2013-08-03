with Ada.Text_IO ;       use Ada.Text_IO ;
with Ada.Command_Line ;  use Ada.Command_Line ;
with Input_Sources.File; use Input_Sources.File ;
with DOM.Readers;        use DOM.Readers;
with DOM.Core;           use DOM.Core;

procedure Wasabee_File is
   Input : File_Input ;
   Reader : Tree_Reader ;
   Doc    : Document ;
begin
   Open(Argument(1), Input);
   Parse(Reader, Input);
   Close(Input);
   Doc := Get_Tree(Reader);

   Parse(Reader, Input) ;

   Free(Reader) ;
end ;
