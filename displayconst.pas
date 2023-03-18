{Copyright (c) 2023 Stephan Breer

This software is provided 'as-is', without any express or implied warranty. In no event will the authors be held liable for any damages arising from the use of this software.

Permission is granted to anyone to use this software for any purpose, including commercial applications, and to alter it and redistribute it freely, subject to the following restrictions:

    1. The origin of this software must not be misrepresented; you must not claim that you wrote the original software. If you use this software in a product, an acknowledgment in the product documentation would be appreciated but is not required.

    2. Altered source versions must be plainly marked as such, and must not be misrepresented as being the original software.

    3. This notice may not be removed or altered from any source distribution.
}

unit DisplayConst;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

var
  PicFilterStr: string;

const
  HMARGIN = 16;
  VMARGIN = 16;
  PORTRAITWIDTH = 250;
  PORTRAITHEIGHT = 250;
  INITIATIVEWIDTH = 200;
  INITIATIVEHEIGHT = 200;
  FRAMESIZE = 7;

  SQRT2 = 1.4142135623;
  SQRT05 = 0.7071067811;

  PICFILEFILTER = '*.jpg;*.jpeg;*.png;*.bmp;*.webp';
  PICFILEFILTEREXWEBP = '*.jpg;*.jpeg;*.png;*.bmp';

  SAVESECTIONMAP = 'Map';
  SAVESECTIONGRID = 'Grid';
  SAVESECTIONPORTRAIT = 'Portrait';
  SAVESECTIONINITIATIVE = 'Initiative';
  SAVESECTIONTOKENS = 'Tokens';

implementation

var WebpHandle: TLibHandle;

initialization

  // Check if we can load webp-files
  PicFilterStr := PICFILEFILTER;
  WebpHandle := LoadLibrary('libwebp.dll');
  if WebpHandle = NilHandle then
    PicFilterStr := PICFILEFILTEREXWEBP
  else
    UnloadLibrary(WebpHandle);

finalization

end.

