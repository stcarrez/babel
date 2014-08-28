-----------------------------------------------------------------------
--  babel-files-signatures -- Signatures calculation
--  Copyright (C) 2014 Stephane.Carrez
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

package body Babel.Files.Signatures is

   --  ------------------------------
   --  Compute the SHA1 signature of the data stored in the buffer.
   --  ------------------------------
   procedure Sha1 (Buffer : in Babel.Files.Buffers.Buffer;
                   Result : out Util.Encoders.SHA1.Hash_Array) is
      Ctx  : Util.Encoders.SHA1.Context;
   begin
      Util.Encoders.SHA1.Update (Ctx, Buffer.Data (Buffer.Data'First .. Buffer.Last));
      Util.Encoders.SHA1.Finish (Ctx, Result);
   end Sha1;

end Babel.Files.Signatures;
