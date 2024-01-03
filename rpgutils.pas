{Copyright (c) 2023 Stephan Breer

This software is provided 'as-is', without any express or implied warranty. In no event will the authors be held liable for any damages arising from the use of this software.

Permission is granted to anyone to use this software for any purpose, including commercial applications, and to alter it and redistribute it freely, subject to the following restrictions:

    1. The origin of this software must not be misrepresented; you must not claim that you wrote the original software. If you use this software in a product, an acknowledgment in the product documentation would be appreciated but is not required.

    2. Altered source versions must be plainly marked as such, and must not be misrepresented as being the original software.

    3. This notice may not be removed or altered from any source distribution.
}

unit RPGUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmapTypes;

function Map(Val, InValStart, InValEnd, OutValStart, OutValEnd: Double): Double;

function Bias(Time, pBias: Double): Double;

function Gain(Time, pGain: Double): Double;

function QuadFalloff(Val, MaxVal: Double): Double;

function CubicPulse(Val, PulseCenter, PulseWidth: Double): Double;

// Easings
type TEasingType = (etLinear,
                    etOutQuad,
                    etInQuad,
                    etInOutQuad,
                    etInCubic,
                    etOutCubic,
                    etInOutCubic,
                    etInQuart,
                    etOutQuart,
                    etInOutQuart,
                    etInQuint,
                    etOutQuint,
                    etInOutQuint,
                    etInSine,
                    etOutSine,
                    etInOutSine,
                    etInExpo,
                    etOutExpo,
                    etInOutExpo,
                    etInCirc,
                    etOutCirc,
                    etInOutCirc,
                    etInElastic,
                    etOutElastic,
                    etInOutElastic,
                    etInBack,
                    etOutBack,
                    etInOutBack,
                    etInBounce,
                    etOutBounce,
                    etInOutBounce,
                    etSmoothStep,
                    etSmootherStep);

function Ease(Time, StartVal, ChangeAmt, Duration: Double; eType: TEasingType): Double;

function ClampedEase(MinPos, MaxPos, CurPos: Double; eType: TEasingType): Double;


function SmoothMin(v1, v2, k: Double): Double;

function MixByte(b1, b2: Byte; s: Single): Byte;

function MixPixel(p1, p2: TBGRAPixel; s: Single): TBGRAPixel;

function GetIntersection_RaySegment(rO, rD, s1, s2: TPointF; var int: TPointF): Boolean;

function GetPointSideOfLine(L1, L2, pnt: TPoint): Integer;

implementation

uses
  Math;


function Map(Val, InValStart, InValEnd, OutValStart, OutValEnd: Double): Double;
begin
  Result := OutValStart + (OutValEnd - OutValStart) * ((Val - InValStart) / (InValEnd - InValStart));
end;

function Bias(Time, pBias: Double): Double;
begin
  Result := Time / ((((1.0 / EnsureRange(pBias, 1e-7, 1)) - 2.0) * (1.0 - Time)) + 1.0);
end;

function Gain(Time, pGain: Double): Double;
begin
  if time < 0.5 then
    Result := Bias(time * 2, pGain) / 2
  else
    Result := Bias(time * 2 - 1, 1 - pGain) / 2 + 0.5;
end;

function QuadFalloff(Val, MaxVal: Double): Double;
begin
  // Val >= 0; MaxVal > 0
  Val := 1 / Sqr(Val + 1);
  MaxVal := 1 / Sqr(MaxVal + 1);
  Result := (Val - MaxVal) / (1 - MaxVal);
end;

function CubicPulse(Val, PulseCenter, PulseWidth: Double): Double;
begin
  Val := Abs(Val - PulseCenter);
  if Val > PulseWidth then
    Exit(0);
  Val := Val / PulseWidth;
  Result := Ease(Val, 1, -1, 1, etSmoothStep);
end;

function MixByte(b1, b2: Byte; s: Single): Byte;
begin
  Result := EnsureRange(Round(b1 + s * (b2 - b1)), 0, 255);
end;

function MixPixel(p1, p2: TBGRAPixel; s: Single): TBGRAPixel;
begin
  Result := BGRA(MixByte(p1.red, p2.Red, s),
                 MixByte(p1.green, p2.green, s),
                 MixByte(p1.blue, p2.blue, s),
                 MixByte(p1.alpha, p2.alpha, s));
end;

function SmoothMin(v1, v2, k: Double): Double;
begin
  Result := Min(v1, v2) - Sqr(Max(k - Abs(v1 - v2), 0) / k) * k / 4;
end;

function GetIntersection_RaySegment(rO, rD, s1, s2: TPointF; var int: TPointF): Boolean;
var d, s, t: Double;
  function Cross2(p1, p2: TPointF): Double;
  begin
    Result := (p1.X * p2.Y) - (p1.Y * p2.X);
  end;
begin
  d := Cross2(s2 - s1, rD);
  if not SameValue(d, 0) then
  begin
    s := Cross2(rO - s1, rD) / d;
    t := Cross2(rO - s1, s2 - s1) / d;
    int := rO + rD * t;
    Result := (t >= 0) and (CompareValue(s, 0, 1E-5) >= 0) and (CompareValue(s, 1, 1E-5) <= 0);
  end
  else
  begin
    // d = 0 => Ray and Segment are parallel
    int := rO;
    Result := False;
  end;

end;

function GetPointSideOfLine(L1, L2, pnt: TPoint): Integer;
begin
  // -1: Left of the line; 0: On the line; +1: Right of the line
  Result := Sign((Pnt.Y - L1.Y) * (L2.X - L1.X) - (L2.Y - L1.Y) * (Pnt.X - L1.X));
end;

{ Easing-Functions }

// probably won't need all of these for this project, but wth, I ported them, I'm going to use them

function EaseLinear(Time, StartVal, ChangeAmt, Duration: Double): Double;
begin
  Result := ChangeAmt * (Time / Duration) + StartVal;
end;

function EaseInQuad(Time, StartVal, ChangeAmt, Duration: Double): Double;
begin
  Result := ChangeAmt * Sqr(Time / Duration) + StartVal;
end;

function EaseOutQuad(Time, StartVal, ChangeAmt, Duration: Double): Double;
begin
  Result := -ChangeAmt * (Time / Duration) * ((Time / Duration) - 2) + StartVal;
end;

function EaseInOutQuad(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration * 2;
  if t < 1 then
    Result := ChangeAmt * 0.5 * Sqr(t) + StartVal
  else
    Result := -ChangeAmt * 0.5 * ((t - 1) * (t - 3) - 1) + StartVal;
end;

function EaseInCubic(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration;
  Result := ChangeAmt * t * t * t + StartVal;
end;

function EaseOutCubic(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration - 1;
  Result := ChangeAmt * (t * t * t + 1) + StartVal;
end;

function EaseInOutCubic(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration * 2;
  if t < 1 then
    Result := ChangeAmt * 0.5 * t * t * t + StartVal
  else
    Result := ChangeAmt * 0.5 * ((t - 2) * (t - 2) * (t - 2) + 2) + StartVal;
end;

function EaseInQuart(Time, StartVal, ChangeAmt, Duration: Double): Double;
begin
  Result := ChangeAmt * Sqr(Sqr(Time / Duration)) + StartVal;
end;

function EaseOutQuart(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration - 1;
  Result := -ChangeAmt * (Sqr(Sqr(t)) - 1) + StartVal;
end;

function EaseInOutQuart(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration * 2;
  if t < 1 then
    Result := ChangeAmt * 0.5 * Sqr(Sqr(t)) + StartVal
  else
    Result := -ChangeAmt * 0.5 * (Sqr(Sqr(t - 2)) - 2) + StartVal;
end;

function EaseInQuint(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration;
  Result := ChangeAmt * Sqr(Sqr(t)) * t + StartVal;
end;

function EaseOutQuint(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration - 1;
  Result := ChangeAmt * (Sqr(sqr(t)) * t + 1) + StartVal;
end;

function EaseInOutQuint(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration * 2;
  if t < 1 then
    Result := ChangeAmt * 0.5 * Sqr(Sqr(t)) * t + StartVal
  else
    Result := ChangeAmt * 0.5 * (Sqr(Sqr(t - 2)) * (t - 2) + 2) + StartVal;
end;

function EaseInSine(Time, StartVal, ChangeAmt, Duration: Double): Double;
begin
  Result := -ChangeAmt * Cos(Time / Duration * (PI / 2)) + ChangeAmt + StartVal;
end;

function EaseOutSine(Time, StartVal, ChangeAmt, Duration: Double): Double;
begin
  Result := ChangeAmt * Sin(Time / Duration * (PI / 2)) + StartVal;
end;

function EaseInOutSine(Time, StartVal, ChangeAmt, Duration: Double): Double;
begin
  Result := -ChangeAmt * 0.5 * (Cos(PI * Time / Duration) - 1) + StartVal;
end;

function EaseInExpo(Time, StartVal, ChangeAmt, Duration: Double): Double;
begin
  if SameValue(Time, 0.0) then
    Result := StartVal
  else
    Result := ChangeAmt * Power(2, 10 * (Time / Duration - 1)) + StartVal;
end;

function EaseOutExpo(Time, StartVal, ChangeAmt, Duration: Double): Double;
begin
  if SameValue(Time, Duration) then
    Result := StartVal + ChangeAmt
  else
    Result := ChangeAmt * (-Power(2, -10 * Time / Duration) + 1) + StartVal;
end;

function EaseInOutExpo(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration * 2;
  if SameValue(Time, 0) then
    Result := StartVal
  else if SameValue(Time, Duration) then
    Result := StartVal + ChangeAmt
  else if (t / 2) < 1 then
    Result := ChangeAmt * 0.5 * Power(2, 10 * (t - 1)) + StartVal
  else
    Result := ChangeAmt * 0.5 * (-Power(2, -10 * (t - 1)) + 2) + StartVal;
end;

function EaseInCirc(Time, StartVal, ChangeAmt, Duration: Double): Double;
begin
  Result := -ChangeAmt * (Sqrt(1 - Sqr(Time / Duration)) - 1) + StartVal;
end;

function EaseOutCirc(Time, StartVal, ChangeAmt, Duration: Double): Double;
begin
  Result := ChangeAmt * Sqrt(1 - Sqr(Time / Duration - 1)) + StartVal;
end;

function EaseInOutCirc(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration * 2;
  if t < 1 then
    Result := -ChangeAmt * 0.5 * (Sqrt(1 - Sqr(t)) - 1) + StartVal
  else
    Result := ChangeAmt * 0.5 * (Sqrt(1 - Sqr(t - 2)) + 1) + StartVal;
end;

function EaseInElastic(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration;
  Result := ChangeAmt * Sin(13 * PI / 2 * t) * Power(2, 10 * (t - 1)) + StartVal;
end;

function EaseOutElastic(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration;
  Result := ChangeAmt * (Sin(-13 * PI / 2 * (t + 1)) * Power(2, -10 * t) + 1) + StartVal;
end;

function EaseInOutElastic(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration;
  if t < 0.5 then
    Result := ChangeAmt * 0.5 * Sin(13 * PI * t) * Power(2, 10 * ((2 * t) - 1)) + StartVal
  else
    Result := ChangeAmt * 0.5 * (Sin(-13 * PI * t) * Power(2, -10 * (2 * t - 1)) + 2) + StartVal;
end;

function EaseInBack(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration;
  Result := ChangeAmt * (Sqr(t) * t - t * Sin(t * PI)) + StartVal;
end;

function EaseOutBack(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := 1 - Time / Duration;
  Result := ChangeAmt * (1 - (Sqr(t) * t - t * Sin(t * PI))) + StartVal;
end;

function EaseInOutBack(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  if (Time / Duration) < 0.5 then
  begin
    t := 2 * Time / Duration;
    Result := ChangeAmt * 0.5 * (Sqr(t) * t - t * Sin(t * PI)) + StartVal;
  end
  else
  begin
    t := 2 - 2 * Time / Duration;
    Result := ChangeAmt * (0.5 * (1 - (Sqr(t) * t - t * Sin(t * PI))) + 0.5) + StartVal;
  end;
end;

function EaseOutBounce(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration;
  if t < (1 / 2.75) then
    Result := ChangeAmt * (7.5625 * Sqr(t)) + StartVal
  else if t < (2 / 2.75) then
    Result := ChangeAmt * (7.5625 * Sqr(t - (1.5 / 2.75)) + 0.75) + StartVal
  else if t < (2.5 / 2.75) then
    Result := ChangeAmt * (7.5625 * Sqr(t - (2.25 / 2.75)) + 0.9375) + StartVal
  else
    Result := ChangeAmt * (7.5625 * Sqr(t - (2.625 / 2.75)) + 0.984375) + StartVal;
end;

function EaseInBounce(Time, StartVal, ChangeAmt, Duration: Double): Double;
begin
  Result := ChangeAmt - EaseOutBounce(Duration - Time, 0, ChangeAmt, Duration) + StartVal;
end;

function EaseInOutBounce(Time, StartVal, ChangeAmt, Duration: Double): Double;
begin
  if Time < (Duration / 2) then
    Result := EaseInBounce(Time * 2, 0, ChangeAmt, Duration) * 0.5 + StartVal
  else
    Result := EaseOutBounce(Time * 2 - Duration, 0, ChangeAmt, Duration) * 0.5 + ChangeAmt * 0.5 + StartVal;
end;

function EaseSmoothStep(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := EnsureRange(Time / Duration, 0, 1);
  Result := ChangeAmt * (t * t * (3 - 2 * t)) + StartVal;
end;

function EaseSmootherStep(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := EnsureRange(Time / Duration, 0, 1);
  Result := ChangeAmt * (t * t * t * (t * (t * 6 - 15) + 10)) + StartVal;
end;

function Ease(Time, StartVal, ChangeAmt, Duration: Double; eType: TEasingType): Double;
begin
  case eType of
    etLinear      : Result := EaseLinear(Time, StartVal, ChangeAmt, Duration);
    etOutQuad     : Result := EaseOutQuad(Time, StartVal, ChangeAmt, Duration);
    etInQuad      : Result := EaseInQuad(Time, StartVal, ChangeAmt, Duration);
    etInOutQuad   : Result := EaseInOutQuad(Time, StartVal, ChangeAmt, Duration);
    etInCubic     : Result := EaseInCubic(Time, StartVal, ChangeAmt, Duration);
    etOutCubic    : Result := EaseOutCubic(Time, StartVal, ChangeAmt, Duration);
    etInOutCubic  : Result := EaseInOutCubic(Time, StartVal, ChangeAmt, Duration);
    etInQuart     : Result := EaseInQuart(Time, StartVal, ChangeAmt, Duration);
    etOutQuart    : Result := EaseOutQuart(Time, StartVal, ChangeAmt, Duration);
    etInOutQuart  : Result := EaseInOutQuart(Time, StartVal, ChangeAmt, Duration);
    etInQuint     : Result := EaseInQuint(Time, StartVal, ChangeAmt, Duration);
    etOutQuint    : Result := EaseOutQuint(Time, StartVal, ChangeAmt, Duration);
    etInOutQuint  : Result := EaseInOutQuint(Time, StartVal, ChangeAmt, Duration);
    etInSine      : Result := EaseInSine(Time, StartVal, ChangeAmt, Duration);
    etOutSine     : Result := EaseOutSine(Time, StartVal, ChangeAmt, Duration);
    etInOutSine   : Result := EaseInOutSine(Time, StartVal, ChangeAmt, Duration);
    etInExpo      : Result := EaseInExpo(Time, StartVal, ChangeAmt, Duration);
    etOutExpo     : Result := EaseOutExpo(Time, StartVal, ChangeAmt, Duration);
    etInOutExpo   : Result := EaseInOutExpo(Time, StartVal, ChangeAmt, Duration);
    etInCirc      : Result := EaseInCirc(Time, StartVal, ChangeAmt, Duration);
    etOutCirc     : Result := EaseOutCirc(Time, StartVal, ChangeAmt, Duration);
    etInOutCirc   : Result := EaseInOutCirc(Time, StartVal, ChangeAmt, Duration);
    etInElastic   : Result := EaseInElastic(Time, StartVal, ChangeAmt, Duration);
    etOutElastic  : Result := EaseOutElastic(Time, StartVal, ChangeAmt, Duration);
    etInOutElastic: Result := EaseInOutElastic(Time, StartVal, ChangeAmt, Duration);
    etInBack      : Result := EaseInBack(Time, StartVal, ChangeAmt, Duration);
    etOutBack     : Result := EaseOutBack(Time, StartVal, ChangeAmt, Duration);
    etInOutBack   : Result := EaseInOutBack(Time, StartVal, ChangeAmt, Duration);
    etInBounce    : Result := EaseInBounce(Time, StartVal, ChangeAmt, Duration);
    etOutBounce   : Result := EaseOutBounce(Time, StartVal, ChangeAmt, Duration);
    etInOutBounce : Result := EaseInOutBounce(Time, StartVal, ChangeAmt, Duration);
    etSmoothStep  : Result := EaseSmoothStep(Time, StartVal, ChangeAmt, Duration);
    etSmootherStep: Result := EaseSmootherStep(Time, StartVal, ChangeAmt, Duration);
  else
    Result := 0;
  end;
end;

function ClampedEase(MinPos, MaxPos, CurPos: Double; eType: TEasingType): Double;
begin
  if CurPos <= MinPos then
    Result := 0
  else if CurPos >= MaxPos then
    Result := 1
  else
    Result := Ease(CurPos - MinPos, 0, 1, MaxPos - MinPos, eType);
end;

end.

