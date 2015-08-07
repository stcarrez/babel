-----------------------------------------------------------------------
--  babel-archivers -- File archiver
--  Copyright (C) 2015 Stephane.Carrez
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

with Ada.Finalization;
with Babel.Streams;
package Babel.Archivers is

   type Archiver_Type is abstract tagged limited private;
   type Archiver_Access is access all Archiver_Type'Class;

   --  Create an archive that will write on the given data stream.
   procedure Create (Archiver : in out Archiver_Type;
                     Stream   : in Babel.Streams.Stream_Access) is abstract;

   --  Append the file at the end of the archiver.
   procedure Append_File (Archiver : in out Archiver_Type;
                          File     : in Babel.Files.File_Type) is abstract;

   --  Open the archiver to read the given data stream.
   procedure Open (Archiver : in out Archiver_Type;
                   Stream   : in Babel.Streams.Stream_Access) is abstract;

   --  Returns true if the archiver contains another file entry.
   function Has_Element (Archiver : in Archiver_Type) return Boolean is abstract;

   --  Read the next file entry from the archive.
   procedure Next (Archiver : in out Archiver_Type) is abstract;

   --  Get the current file entry from the archive.
   function Element (Archiver : in Archiver_Type) return Babel.Files.File_Type is abstract;

   --  Open a stream to read the content of the file stored in the archive.
   procedure Open_File (Archiver : in out Archiver_Type;
                        Stream   : out Babel.Streams.Stream_Access) is abstract;

   --  Flush the data stream.
   procedure Flush (Archiver : in out Archiver_Type) is null;

private

   type Archiver_Type is abstract limited new Ada.Finalization.Limited_Controlled with record
      Input   : Babel.Streams.Stream_Access;
      Output  : Babel.Streams.Stream_Access;
   end record;

   --  Release the archive internal storage.
   overriding
   procedure Finalize (Archiver : in out Archiver_Type);

end Babel.Archivers;
