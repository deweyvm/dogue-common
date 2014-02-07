package com.deweyvm.dogue.common.data

import scala.collection.mutable.ArrayBuffer
import com.deweyvm.dogue.common.Implicits
import Implicits._

//code page 437
//uppercase special characters are suffixed with _u due to the possible error:
// "Code$Ä$ differs only in case from Code$ä$. Such classes will overwrite one
// another on case-insensitive filesystems."

object Code {
  val All = ArrayBuffer[Code]()
  val ☺          = Code(  1, '☺', '\u263A')
  val ☻          = Code(  2, '☻', '\u263B')
  val ♥          = Code(  3, '♥', '\u2665')
  val ♦          = Code(  4, '♦', '\u2666')
  val ♣          = Code(  5, '♣', '\u2663')
  val ♠          = Code(  6, '♠', '\u2660')
  val ●          = Code(  7, '●', '\u2022')
  val ◘          = Code(  8, '◘', '\u25D8')
  val ◦          = Code(  9, '◦', '\u25CB')
  val ◙          = Code( 10, '◙', '\u25D9')
  val ♂          = Code( 11, '♂', '\u2642')
  val ♀          = Code( 12, '♀', '\u2640')
  val ♪          = Code( 13, '♪', '\u266A')
  val ♫          = Code( 14, '♫', '\u266B')
  val ☼          = Code( 15, '☼', '\u263C')
  val ►          = Code( 16, '►', '\u25BA')
  val ◄          = Code( 17, '◄', '\u25C4')
  val ↕          = Code( 18, '↕', '\u2195')
  val `‼`        = Code( 19, '‼', '\u203C')
  val ¶          = Code( 20, '¶', '\u00B6')
  val §          = Code( 21, '§', '\u00A7')
  val `‗`        = Code( 22, '‗', '\u25AC')
  val ↨          = Code( 23, '↨', '\u21A8')
  val ↑          = Code( 24, '↑', '\u2191')
  val ↓          = Code( 25, '↓', '\u2193')
  val →          = Code( 26, '→', '\u2192')
  val `←`        = Code( 27, '←', '\u2190')
  val ∟          = Code( 28, '∟', '\u221F')
  val ↔          = Code( 29, '↔', '\u2194')
  val ▲          = Code( 30, '▲', '\u25B2')
  val ▼          = Code( 31, '▼', '\u25BC')
  val ` `        = Code( 32, ' ', '\u0020')
  val !          = Code( 33, '!', '\u0021')
  val `"`        = Code( 34, '"', '\u0022')
  val `#`        = Code( 35, '#', '\u0023')
  val $          = Code( 36, '$', '\u0024')
  val %          = Code( 37, '%', '\u0025')
  val &          = Code( 38, '&', '\u0026')
  val `'`        = Code( 39,'\'', '\u0027')
  val `(`        = Code( 40, '(', '\u0028')
  val `)`        = Code( 41, ')', '\u0029')
  val *          = Code( 42, '*', '\u002A')
  val +          = Code( 43, '+', '\u002B')
  val `,`        = Code( 44, ',', '\u002C')
  val -          = Code( 45, '-', '\u002D')
  val `.`        = Code( 46, '.', '\u002E')
  val /          = Code( 47, '/', '\u002F')
  val `0`        = Code( 48, '0', '\u0030')
  val `1`        = Code( 49, '1', '\u0031')
  val `2`        = Code( 50, '2', '\u0032')
  val `3`        = Code( 51, '3', '\u0033')
  val `4`        = Code( 52, '4', '\u0034')
  val `5`        = Code( 53, '5', '\u0035')
  val `6`        = Code( 54, '6', '\u0036')
  val `7`        = Code( 55, '7', '\u0037')
  val `8`        = Code( 56, '8', '\u0038')
  val `9`        = Code( 57, '9', '\u0039')
  val `:`        = Code( 58, ':', '\u003A')
  val `;`        = Code( 59, ';', '\u003B')
  val <          = Code( 60, '<', '\u003C')
  val `=`        = Code( 61, '=', '\u003D')
  val >          = Code( 62, '>', '\u003E')
  val ?          = Code( 63, '?', '\u003F')
  val `@`        = Code( 64, '@', '\u0040')
  val A          = Code( 65, 'A', '\u0041')
  val B          = Code( 66, 'B', '\u0042')
  val C          = Code( 67, 'C', '\u0043')
  val D          = Code( 68, 'D', '\u0044')
  val E          = Code( 69, 'E', '\u0045')
  val F          = Code( 70, 'F', '\u0046')
  val G          = Code( 71, 'G', '\u0047')
  val H          = Code( 72, 'H', '\u0048')
  val I          = Code( 73, 'I', '\u0049')
  val J          = Code( 74, 'J', '\u004A')
  val K          = Code( 75, 'K', '\u004B')
  val L          = Code( 76, 'L', '\u004C')
  val M          = Code( 77, 'M', '\u004D')
  val N          = Code( 78, 'N', '\u004E')
  val O          = Code( 79, 'O', '\u004F')
  val P          = Code( 80, 'P', '\u0050')
  val Q          = Code( 81, 'Q', '\u0051')
  val R          = Code( 82, 'R', '\u0052')
  val S          = Code( 83, 'S', '\u0053')
  val T          = Code( 84, 'T', '\u0054')
  val U          = Code( 85, 'U', '\u0055')
  val V          = Code( 86, 'V', '\u0056')
  val W          = Code( 87, 'W', '\u0057')
  val X          = Code( 88, 'X', '\u0058')
  val Y          = Code( 89, 'Y', '\u0059')
  val Z          = Code( 90, 'Z', '\u005A')
  val `[`        = Code( 91, '[', '\u005B')
  val backslash  = Code( 92, '\\', '\\')
  val `]`        = Code( 93, ']', '\u005D')
  val ^          = Code( 94, '^', '\u005E')
  val underscore = Code( 95, '_', '\u005F')
  val grave      = Code( 96, '`', '\u0060')
  val a          = Code( 97, 'a', '\u0061')
  val b          = Code( 98, 'b', '\u0062')
  val c          = Code( 99, 'c', '\u0063')
  val d          = Code(100, 'd', '\u0064')
  val e          = Code(101, 'e', '\u0065')
  val f          = Code(102, 'f', '\u0066')
  val g          = Code(103, 'g', '\u0067')
  val h          = Code(104, 'h', '\u0068')
  val i          = Code(105, 'i', '\u0069')
  val j          = Code(106, 'j', '\u006A')
  val k          = Code(107, 'k', '\u006B')
  val l          = Code(108, 'l', '\u006C')
  val m          = Code(109, 'm', '\u006D')
  val n          = Code(110, 'n', '\u006E')
  val o          = Code(111, 'o', '\u006F')
  val p          = Code(112, 'p', '\u0070')
  val q          = Code(113, 'q', '\u0071')
  val r          = Code(114, 'r', '\u0072')
  val s          = Code(115, 's', '\u0073')
  val t          = Code(116, 't', '\u0074')
  val u          = Code(117, 'u', '\u0075')
  val v          = Code(118, 'v', '\u0076')
  val w          = Code(119, 'w', '\u0077')
  val x          = Code(120, 'x', '\u0078')
  val y          = Code(121, 'y', '\u0079')
  val z          = Code(122, 'z', '\u007A')
  val `{`        = Code(123, '{', '\u007B')
  val |          = Code(124, '|', '\u007C')
  val `}`        = Code(125, '}', '\u007D')
  val ~          = Code(126, '~', '\u007E')
  val ⌂          = Code(127, '⌂', '\u007F')
  val Ç_u        = Code(128, 'Ç', '\u00C7')
  val ü          = Code(129, 'ü', '\u00FC')
  val é          = Code(130, 'é', '\u00E9')
  val â          = Code(131, 'â', '\u00E2')
  val ä          = Code(132, 'ä', '\u00E4')
  val à          = Code(133, 'à', '\u00E0')
  val å          = Code(134, 'å', '\u00E5')
  val ç          = Code(135, 'ç', '\u00E7')
  val ê          = Code(136, 'ê', '\u00EA')
  val ë          = Code(137, 'ë', '\u00EB')
  val è          = Code(138, 'è', '\u00E8')
  val ï          = Code(139, 'ï', '\u00EF')
  val î          = Code(140, 'î', '\u00EE')
  val ì          = Code(141, 'ì', '\u00EC')
  val Ä_u        = Code(142, 'Ä', '\u00C4')
  val Å_u        = Code(143, 'Å', '\u00C5')
  val É_u        = Code(144, 'É', '\u00C9')
  val æ          = Code(145, 'æ', '\u00E6')
  val Æ_u        = Code(146, 'Æ', '\u00C6')
  val ô          = Code(147, 'ô', '\u00F4')
  val ö          = Code(148, 'ö', '\u00F6')
  val ò          = Code(149, 'ò', '\u00F2')
  val û          = Code(150, 'û', '\u00FB')
  val ù          = Code(151, 'ù', '\u00F9')
  val ÿ          = Code(152, 'ÿ', '\u00FF')
  val Ö_u        = Code(153, 'Ö', '\u00D6')
  val Ü_u        = Code(154, 'Ü', '\u00DC')
  val `¢`        = Code(155, '¢', '\u00A2')
  val `£`        = Code(156, '£', '\u00A3')
  val `¥`        = Code(157, '¥', '\u00A5')
  val `₧`        = Code(158, '₧', '\u20A7')
  val ƒ          = Code(159, 'ƒ', '\u0192')
  val á          = Code(160, 'á', '\u00E1')
  val í          = Code(161, 'í', '\u00ED')
  val ó          = Code(162, 'ó', '\u00F3')
  val ú          = Code(163, 'ú', '\u00FA')
  val ñ          = Code(164, 'ñ', '\u00F1')
  val Ñ_u        = Code(165, 'Ñ', '\u00D1')
  val ª          = Code(166, 'ª', '\u00AA')
  val º          = Code(167, 'º', '\u00BA')
  val `¿`        = Code(168, '¿', '\u00BF')
  val ⌐          = Code(169, '⌐', '\u2310')
  val ¬          = Code(170, '¬', '\u00AC')
  val `½`        = Code(171, '½', '\u00BD')
  val `¼`        = Code(172, '¼', '\u00BC')
  val `¡`        = Code(173, '¡', '\u00A1')
  val `«`        = Code(174, '«', '\u00AB')
  val `»`        = Code(175, '»', '\u00BB')
  val ░          = Code(176, '░', '\u2591')
  val ▒          = Code(177, '▒', '\u2592')
  val ▓          = Code(178, '▓', '\u2593')
  val │          = Code(179, '│', '\u2502')
  val ┤          = Code(180, '┤', '\u2524')
  val ╡          = Code(181, '╡', '\u2561')
  val ╢          = Code(182, '╢', '\u2562')
  val ╖          = Code(183, '╖', '\u2556')
  val ╕          = Code(184, '╕', '\u2555')
  val ╣          = Code(185, '╣', '\u2563')
  val ║          = Code(186, '║', '\u2551')
  val ╗          = Code(187, '╗', '\u2557')
  val ╝          = Code(188, '╝', '\u255D')
  val ╜          = Code(189, '╜', '\u255C')
  val ╛          = Code(190, '╛', '\u255B')
  val ┐          = Code(191, '┐', '\u2510')
  val └          = Code(192, '└', '\u2514')
  val ┴          = Code(193, '┴', '\u2534')
  val ┬          = Code(194, '┬', '\u252C')
  val ├          = Code(195, '├', '\u251C')
  val ─          = Code(196, '─', '\u2500')
  val ┼          = Code(197, '┼', '\u253C')
  val ╞          = Code(198, '╞', '\u255E')
  val ╟          = Code(199, '╟', '\u255F')
  val ╚          = Code(200, '╚', '\u255A')
  val ╔          = Code(201, '╔', '\u2554')
  val ╩          = Code(202, '╩', '\u2569')
  val ╦          = Code(203, '╦', '\u2566')
  val ╠          = Code(204, '╠', '\u2560')
  val ═          = Code(205, '═', '\u2550')
  val ╬          = Code(206, '╬', '\u256C')
  val ╧          = Code(207, '╧', '\u2567')
  val ╨          = Code(208, '╨', '\u2568')
  val ╤          = Code(209, '╤', '\u2564')
  val ╥          = Code(210, '╥', '\u2565')
  val ╙          = Code(211, '╙', '\u2559')
  val ╘          = Code(212, '╘', '\u2558')
  val ╒          = Code(213, '╒', '\u2552')
  val ╓          = Code(214, '╓', '\u2553')
  val ╫          = Code(215, '╫', '\u256B')
  val ╪          = Code(216, '╪', '\u256A')
  val ┘          = Code(217, '┘', '\u2518')
  val ┌          = Code(218, '┌', '\u250C')
  val █          = Code(219, '█', '\u2588')
  val ▄          = Code(220, '▄', '\u2584')
  val ▌          = Code(221, '▌', '\u258C')
  val ▐          = Code(222, '▐', '\u2590')
  val ▀          = Code(223, '▀', '\u2580')
  val α          = Code(224, 'α', '\u03B1')
  val β          = Code(225, 'β', '\u00DF')
  val Γ          = Code(226, 'Γ', '\u0393')
  val π          = Code(227, 'π', '\u03C0')
  val Σ_u        = Code(228, 'Σ', '\u03A3')
  val σ          = Code(229, 'σ', '\u03C3')
  val μ          = Code(230, 'μ', '\u00B5')
  val τ          = Code(231, 'τ', '\u03C4')
  val Φ          = Code(232, 'Φ', '\u03A6')
  val Θ          = Code(233, 'Θ', '\u0398')
  val Ω          = Code(234, 'Ω', '\u03A9')
  val δ          = Code(235, 'δ', '\u03B4')
  val ∞          = Code(236, '∞', '\u221E')
  val φ          = Code(237, 'φ', '\u03C6')
  val ϵ          = Code(238, 'ϵ', '\u03B5')
  val ∩          = Code(239, '∩', '\u2229')
  val ≡          = Code(240, '≡', '\u2261')
  val ±          = Code(241, '±', '\u00B1')
  val ≥          = Code(242, '≥', '\u2265')
  val ≤          = Code(243, '≤', '\u2264')
  val ⌠          = Code(244, '⌠', '\u2320')
  val ⌡          = Code(245, '⌡', '\u2321')
  val ÷          = Code(246, '÷', '\u00F7')
  val ≈          = Code(247, '≈', '\u2248')
  val °          = Code(248, '°', '\u00B0')
  val ▪          = Code(249, '▪', '\u2219')
  val `·`        = Code(250, '·', '\u00B7')
  val √          = Code(251, '√', '\u221A')
  val ⁿ          = Code(252, 'ⁿ', '\u207F')
  val `²`        = Code(253, '²', '\u00B2')
  val ■          = Code(254, '■', '\u25A0')

  def random:Code = All.getRandom

  def codeToUnicode(code:Char):Char = {
    if (code < 1 || code > 253) {
      '?'
    } else {
      All(code-1).unicode
    }
  }

  def unicodeToCode(s:Char):Code = {
    All.find(_.unicode == s) getOrElse Code.?
  }

}

case class Code(index:Int, char:Char='?', unicode:Char) {
  if (Code.All.length < 255) {
    Code.All += this
    ()
  }

  def rawString = index.toChar.toString

}