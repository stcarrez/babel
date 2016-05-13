-----------------------------------------------------------------------
--  babel-stores -- Storage management
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
with Babel.Files;
with Babel.Files.Buffers;
with Babel.Filters;
with Babel.Streams;
with Babel.Streams.Refs;
package Babel.Stores is

   type Store_Type is limited interface;
   type Store_Type_Access is access all Store_Type'Class;

   --  Open a file in the store to read its content with a stream.
   procedure Open_File (Store  : in out Store_Type;
                        Path   : in String;
                        Stream : out Babel.Streams.Stream_Access) is abstract;

   --  Open a file in the store to read its content with a stream.
   procedure Read_File (Store  : in out Store_Type;
                        Path   : in String;
                        Stream : out Babel.Streams.Refs.Stream_Ref) is abstract;

   --  Write a file in the store with a stream.
   procedure Write_File (Store  : in out Store_Type;
                         Path   : in String;
                         Stream : in Babel.Streams.Refs.Stream_Ref;
                         Mode   : in Babel.Files.File_Mode) is abstract;

   procedure Read (Store : in out Store_Type;
                   Path  : in String;
                   Into  : in out Babel.Files.Buffers.Buffer) is abstract;

   procedure Write (Store : in out Store_Type;
                    Path  : in String;
                    Into  : in Babel.Files.Buffers.Buffer) is abstract;

   procedure Scan (Store  : in out Store_Type;
                   Path   : in String;
                   Into   : in out Babel.Files.File_Container'Class;
                   Filter : in Babel.Filters.Filter_Type'Class) is abstract;

end Babel.Stores;
