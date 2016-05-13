-----------------------------------------------------------------------
--  babel-stores-local -- Store to access local files
--  Copyright (C) 2014, 2015, 2016 Stephane.Carrez
--  Written by Stephane.Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------
with Ada.Strings.Unbounded;
with Util.Streams.Raw;

with Babel.Files.Buffers;
package Babel.Stores.Local is

   type Local_Store_Type is new Babel.Stores.Store_Type with private;

   --  Get the absolute path to access the local file.
   function Get_Absolute_Path (Store : in Local_Store_Type;
                               Path  : in String) return String;

   --  Open a file in the store to read its content with a stream.
   overriding
   procedure Open_File (Store  : in out Local_Store_Type;
                        Path   : in String;
                        Stream : out Babel.Streams.Stream_Access);

   --  Open a file in the store to read its content with a stream.
   overriding
   procedure Read_File (Store  : in out Local_Store_Type;
                        Path   : in String;
                        Stream : out Babel.Streams.Refs.Stream_Ref);

   --  Write a file in the store with a stream.
   overriding
   procedure Write_File (Store  : in out Local_Store_Type;
                         Path   : in String;
                         Stream : in Babel.Streams.Refs.Stream_Ref;
                         Mode   : in Babel.Files.File_Mode);

   overriding
   procedure Read (Store : in out Local_Store_Type;
                   Path  : in String;
                   Into  : in out Babel.Files.Buffers.Buffer);

   overriding
   procedure Write (Store : in out Local_Store_Type;
                    Path  : in String;
                    Into  : in Babel.Files.Buffers.Buffer);

   overriding
   procedure Scan (Store  : in out Local_Store_Type;
                   Path   : in String;
                   Into   : in out Babel.Files.File_Container'Class;
                   Filter : in Babel.Filters.Filter_Type'Class);

   procedure Open (Store  : in out Local_Store_Type;
                   Stream : in out Util.Streams.Raw.Raw_Stream;
                   Path   : in String);

   --  Set the root directory for the local store.
   procedure Set_Root_Directory (Store : in out Local_Store_Type;
                                 Path  : in String);

   --  Set the buffer pool to be used by local store.
   procedure Set_Buffers (Store   : in out Local_Store_Type;
                          Buffers : in Babel.Files.Buffers.Buffer_Pool_Access);

private

   type Local_Store_Type is new Babel.Stores.Store_Type with record
      Root_Dir : Ada.Strings.Unbounded.Unbounded_String;
      Pool     : Babel.Files.Buffers.Buffer_Pool_Access;
   end record;

end Babel.Stores.Local;
