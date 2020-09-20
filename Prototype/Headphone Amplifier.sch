<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE eagle SYSTEM "eagle.dtd">
<eagle version="7.7.0">
<drawing>
<settings>
<setting alwaysvectorfont="no"/>
<setting verticaltext="up"/>
</settings>
<grid distance="0.1" unitdist="inch" unit="inch" style="lines" multiple="1" display="no" altdistance="0.05" altunitdist="inch" altunit="inch"/>
<layers>
<layer number="1" name="Top" color="4" fill="1" visible="no" active="no"/>
<layer number="2" name="Route2" color="1" fill="3" visible="no" active="no"/>
<layer number="3" name="Route3" color="4" fill="3" visible="no" active="no"/>
<layer number="4" name="Route4" color="1" fill="4" visible="no" active="no"/>
<layer number="5" name="Route5" color="4" fill="4" visible="no" active="no"/>
<layer number="6" name="Route6" color="1" fill="8" visible="no" active="no"/>
<layer number="7" name="Route7" color="4" fill="8" visible="no" active="no"/>
<layer number="8" name="Route8" color="1" fill="2" visible="no" active="no"/>
<layer number="9" name="Route9" color="4" fill="2" visible="no" active="no"/>
<layer number="10" name="Route10" color="1" fill="7" visible="no" active="no"/>
<layer number="11" name="Route11" color="4" fill="7" visible="no" active="no"/>
<layer number="12" name="Route12" color="1" fill="5" visible="no" active="no"/>
<layer number="13" name="Route13" color="4" fill="5" visible="no" active="no"/>
<layer number="14" name="Route14" color="1" fill="6" visible="no" active="no"/>
<layer number="15" name="Route15" color="4" fill="6" visible="no" active="no"/>
<layer number="16" name="Bottom" color="1" fill="1" visible="no" active="no"/>
<layer number="17" name="Pads" color="2" fill="1" visible="no" active="no"/>
<layer number="18" name="Vias" color="2" fill="1" visible="no" active="no"/>
<layer number="19" name="Unrouted" color="6" fill="1" visible="no" active="no"/>
<layer number="20" name="Dimension" color="15" fill="1" visible="no" active="no"/>
<layer number="21" name="tPlace" color="7" fill="1" visible="no" active="no"/>
<layer number="22" name="bPlace" color="7" fill="1" visible="no" active="no"/>
<layer number="23" name="tOrigins" color="15" fill="1" visible="no" active="no"/>
<layer number="24" name="bOrigins" color="15" fill="1" visible="no" active="no"/>
<layer number="25" name="tNames" color="7" fill="1" visible="no" active="no"/>
<layer number="26" name="bNames" color="7" fill="1" visible="no" active="no"/>
<layer number="27" name="tValues" color="7" fill="1" visible="no" active="no"/>
<layer number="28" name="bValues" color="7" fill="1" visible="no" active="no"/>
<layer number="29" name="tStop" color="7" fill="3" visible="no" active="no"/>
<layer number="30" name="bStop" color="7" fill="6" visible="no" active="no"/>
<layer number="31" name="tCream" color="7" fill="4" visible="no" active="no"/>
<layer number="32" name="bCream" color="7" fill="5" visible="no" active="no"/>
<layer number="33" name="tFinish" color="6" fill="3" visible="no" active="no"/>
<layer number="34" name="bFinish" color="6" fill="6" visible="no" active="no"/>
<layer number="35" name="tGlue" color="7" fill="4" visible="no" active="no"/>
<layer number="36" name="bGlue" color="7" fill="5" visible="no" active="no"/>
<layer number="37" name="tTest" color="7" fill="1" visible="no" active="no"/>
<layer number="38" name="bTest" color="7" fill="1" visible="no" active="no"/>
<layer number="39" name="tKeepout" color="4" fill="11" visible="no" active="no"/>
<layer number="40" name="bKeepout" color="1" fill="11" visible="no" active="no"/>
<layer number="41" name="tRestrict" color="4" fill="10" visible="no" active="no"/>
<layer number="42" name="bRestrict" color="1" fill="10" visible="no" active="no"/>
<layer number="43" name="vRestrict" color="2" fill="10" visible="no" active="no"/>
<layer number="44" name="Drills" color="7" fill="1" visible="no" active="no"/>
<layer number="45" name="Holes" color="7" fill="1" visible="no" active="no"/>
<layer number="46" name="Milling" color="3" fill="1" visible="no" active="no"/>
<layer number="47" name="Measures" color="7" fill="1" visible="no" active="no"/>
<layer number="48" name="Document" color="7" fill="1" visible="no" active="no"/>
<layer number="49" name="Reference" color="7" fill="1" visible="no" active="no"/>
<layer number="51" name="tDocu" color="7" fill="1" visible="no" active="no"/>
<layer number="52" name="bDocu" color="7" fill="1" visible="no" active="no"/>
<layer number="90" name="Modules" color="5" fill="1" visible="yes" active="yes"/>
<layer number="91" name="Nets" color="2" fill="1" visible="yes" active="yes"/>
<layer number="92" name="Busses" color="1" fill="1" visible="yes" active="yes"/>
<layer number="93" name="Pins" color="2" fill="1" visible="no" active="yes"/>
<layer number="94" name="Symbols" color="4" fill="1" visible="yes" active="yes"/>
<layer number="95" name="Names" color="7" fill="1" visible="yes" active="yes"/>
<layer number="96" name="Values" color="7" fill="1" visible="yes" active="yes"/>
<layer number="97" name="Info" color="7" fill="1" visible="yes" active="yes"/>
<layer number="98" name="Guide" color="6" fill="1" visible="yes" active="yes"/>
</layers>
<schematic xreflabel="%F%N/%S.%C%R" xrefpart="/%S.%C%R">
<libraries>
<library name="innove-frames">
<description>&lt;b&gt;Frames for Sheets and Layouts&lt;/b&gt;

&lt;p&gt;Simple standardized frames meant to be used in schematics and boards.&lt;/p&gt;

&lt;p&gt;&lt;author&gt;Created by Nathan Campos &amp;lt;nathan@innoveworkshop.com&amp;gt;&lt;/author&gt;&lt;/p&gt;</description>
<packages>
</packages>
<symbols>
<symbol name="A4L">
<description>A4 Landscape Frame</description>
<wire x1="256.6416" y1="3.9116" x2="256.6416" y2="8.9916" width="0.1016" layer="94"/>
<wire x1="256.6416" y1="8.9916" x2="256.6416" y2="14.0716" width="0.1016" layer="94"/>
<wire x1="256.6416" y1="14.0716" x2="256.6416" y2="19.1516" width="0.1016" layer="94"/>
<wire x1="256.6416" y1="19.1516" x2="256.6416" y2="24.2316" width="0.1016" layer="94"/>
<wire x1="171.5516" y1="3.9116" x2="171.5516" y2="24.2316" width="0.1016" layer="94"/>
<wire x1="171.5516" y1="24.2316" x2="215.3666" y2="24.2316" width="0.1016" layer="94"/>
<wire x1="215.3666" y1="24.2316" x2="256.6416" y2="24.2316" width="0.1016" layer="94"/>
<wire x1="245.4656" y1="3.9116" x2="245.4656" y2="8.9916" width="0.1016" layer="94"/>
<wire x1="245.4656" y1="8.9916" x2="256.6416" y2="8.9916" width="0.1016" layer="94"/>
<wire x1="245.4656" y1="8.9916" x2="215.3666" y2="8.9916" width="0.1016" layer="94"/>
<wire x1="215.3666" y1="8.9916" x2="215.3666" y2="3.9116" width="0.1016" layer="94"/>
<wire x1="215.3666" y1="8.9916" x2="215.3666" y2="14.0716" width="0.1016" layer="94"/>
<wire x1="215.3666" y1="14.0716" x2="256.6416" y2="14.0716" width="0.1016" layer="94"/>
<wire x1="215.3666" y1="14.0716" x2="215.3666" y2="19.1516" width="0.1016" layer="94"/>
<wire x1="215.3666" y1="19.1516" x2="256.6416" y2="19.1516" width="0.1016" layer="94"/>
<wire x1="215.3666" y1="19.1516" x2="215.3666" y2="24.2316" width="0.1016" layer="94"/>
<text x="217.2716" y="20.4216" size="2.54" layer="94">&gt;DRAWING_NAME</text>
<text x="217.2716" y="10.2616" size="2.286" layer="94">&gt;LAST_DATE_TIME</text>
<text x="230.6066" y="5.1816" size="2.54" layer="94">&gt;SHEET</text>
<text x="217.0176" y="5.0546" size="2.54" layer="94">Sheet:</text>
<frame x1="0.1016" y1="0.1016" x2="260.4516" y2="179.1716" columns="6" rows="4" layer="94"/>
<text x="217.2716" y="15.3416" size="2.286" layer="94">&gt;AUTHOR</text>
<text x="246.4816" y="5.0546" size="2.54" layer="94">Rev</text>
<text x="253.3396" y="5.0546" size="2.54" layer="94">&gt;REVISION</text>
<rectangle x1="186.563" y1="6.4262" x2="186.7154" y2="6.5786" layer="94"/>
<rectangle x1="194.945" y1="6.4262" x2="195.2498" y2="6.5786" layer="94"/>
<rectangle x1="200.1266" y1="6.4262" x2="200.4314" y2="6.5786" layer="94"/>
<rectangle x1="206.375" y1="6.4262" x2="206.8322" y2="6.5786" layer="94"/>
<rectangle x1="186.563" y1="6.5786" x2="186.8678" y2="6.731" layer="94"/>
<rectangle x1="194.945" y1="6.5786" x2="195.2498" y2="6.731" layer="94"/>
<rectangle x1="199.9742" y1="6.5786" x2="200.7362" y2="6.731" layer="94"/>
<rectangle x1="206.0702" y1="6.5786" x2="207.137" y2="6.731" layer="94"/>
<rectangle x1="186.4106" y1="6.731" x2="186.8678" y2="6.8834" layer="94"/>
<rectangle x1="194.945" y1="6.731" x2="195.4022" y2="6.8834" layer="94"/>
<rectangle x1="200.279" y1="6.731" x2="200.8886" y2="6.8834" layer="94"/>
<rectangle x1="205.9178" y1="6.731" x2="207.2894" y2="6.8834" layer="94"/>
<rectangle x1="186.563" y1="6.8834" x2="186.8678" y2="7.0358" layer="94"/>
<rectangle x1="194.945" y1="6.8834" x2="195.4022" y2="7.0358" layer="94"/>
<rectangle x1="200.5838" y1="6.8834" x2="200.8886" y2="7.0358" layer="94"/>
<rectangle x1="205.7654" y1="6.8834" x2="207.4418" y2="7.0358" layer="94"/>
<rectangle x1="174.8282" y1="7.0358" x2="175.2854" y2="7.1882" layer="94"/>
<rectangle x1="176.0474" y1="7.0358" x2="176.657" y2="7.1882" layer="94"/>
<rectangle x1="177.419" y1="7.0358" x2="178.3334" y2="7.1882" layer="94"/>
<rectangle x1="178.7906" y1="7.0358" x2="179.0954" y2="7.1882" layer="94"/>
<rectangle x1="180.1622" y1="7.0358" x2="180.467" y2="7.1882" layer="94"/>
<rectangle x1="181.229" y1="7.0358" x2="181.5338" y2="7.1882" layer="94"/>
<rectangle x1="181.6862" y1="7.0358" x2="182.753" y2="7.1882" layer="94"/>
<rectangle x1="183.2102" y1="7.0358" x2="183.515" y2="7.1882" layer="94"/>
<rectangle x1="184.277" y1="7.0358" x2="184.5818" y2="7.1882" layer="94"/>
<rectangle x1="185.1914" y1="7.0358" x2="186.1058" y2="7.1882" layer="94"/>
<rectangle x1="186.563" y1="7.0358" x2="187.7822" y2="7.1882" layer="94"/>
<rectangle x1="189.611" y1="7.0358" x2="190.5254" y2="7.1882" layer="94"/>
<rectangle x1="191.2874" y1="7.0358" x2="192.2018" y2="7.1882" layer="94"/>
<rectangle x1="192.659" y1="7.0358" x2="192.9638" y2="7.1882" layer="94"/>
<rectangle x1="193.5734" y1="7.0358" x2="193.8782" y2="7.1882" layer="94"/>
<rectangle x1="194.4878" y1="7.0358" x2="194.7926" y2="7.1882" layer="94"/>
<rectangle x1="194.945" y1="7.0358" x2="196.3166" y2="7.1882" layer="94"/>
<rectangle x1="197.0786" y1="7.0358" x2="197.993" y2="7.1882" layer="94"/>
<rectangle x1="198.4502" y1="7.0358" x2="198.755" y2="7.1882" layer="94"/>
<rectangle x1="199.517" y1="7.0358" x2="199.8218" y2="7.1882" layer="94"/>
<rectangle x1="200.5838" y1="7.0358" x2="201.041" y2="7.1882" layer="94"/>
<rectangle x1="205.7654" y1="7.0358" x2="205.9178" y2="7.1882" layer="94"/>
<rectangle x1="206.0702" y1="7.0358" x2="206.2226" y2="7.1882" layer="94"/>
<rectangle x1="174.6758" y1="7.1882" x2="175.4378" y2="7.3406" layer="94"/>
<rectangle x1="176.0474" y1="7.1882" x2="176.8094" y2="7.3406" layer="94"/>
<rectangle x1="177.2666" y1="7.1882" x2="178.4858" y2="7.3406" layer="94"/>
<rectangle x1="178.7906" y1="7.1882" x2="179.2478" y2="7.3406" layer="94"/>
<rectangle x1="180.0098" y1="7.1882" x2="180.467" y2="7.3406" layer="94"/>
<rectangle x1="181.0766" y1="7.1882" x2="182.9054" y2="7.3406" layer="94"/>
<rectangle x1="183.0578" y1="7.1882" x2="183.515" y2="7.3406" layer="94"/>
<rectangle x1="184.277" y1="7.1882" x2="184.5818" y2="7.3406" layer="94"/>
<rectangle x1="185.039" y1="7.1882" x2="186.2582" y2="7.3406" layer="94"/>
<rectangle x1="186.563" y1="7.1882" x2="187.9346" y2="7.3406" layer="94"/>
<rectangle x1="189.3062" y1="7.1882" x2="190.6778" y2="7.3406" layer="94"/>
<rectangle x1="191.135" y1="7.1882" x2="192.3542" y2="7.3406" layer="94"/>
<rectangle x1="192.659" y1="7.1882" x2="193.1162" y2="7.3406" layer="94"/>
<rectangle x1="193.5734" y1="7.1882" x2="193.8782" y2="7.3406" layer="94"/>
<rectangle x1="194.3354" y1="7.1882" x2="194.7926" y2="7.3406" layer="94"/>
<rectangle x1="195.0974" y1="7.1882" x2="196.469" y2="7.3406" layer="94"/>
<rectangle x1="196.9262" y1="7.1882" x2="198.1454" y2="7.3406" layer="94"/>
<rectangle x1="198.4502" y1="7.1882" x2="198.755" y2="7.3406" layer="94"/>
<rectangle x1="199.517" y1="7.1882" x2="199.9742" y2="7.3406" layer="94"/>
<rectangle x1="200.5838" y1="7.1882" x2="201.041" y2="7.3406" layer="94"/>
<rectangle x1="205.0034" y1="7.1882" x2="205.3082" y2="7.3406" layer="94"/>
<rectangle x1="206.375" y1="7.1882" x2="206.5274" y2="7.3406" layer="94"/>
<rectangle x1="206.6798" y1="7.1882" x2="207.4418" y2="7.3406" layer="94"/>
<rectangle x1="174.6758" y1="7.3406" x2="175.4378" y2="7.493" layer="94"/>
<rectangle x1="176.0474" y1="7.3406" x2="176.8094" y2="7.493" layer="94"/>
<rectangle x1="177.2666" y1="7.3406" x2="177.5714" y2="7.493" layer="94"/>
<rectangle x1="178.181" y1="7.3406" x2="178.6382" y2="7.493" layer="94"/>
<rectangle x1="178.943" y1="7.3406" x2="179.2478" y2="7.493" layer="94"/>
<rectangle x1="180.1622" y1="7.3406" x2="180.467" y2="7.493" layer="94"/>
<rectangle x1="180.9242" y1="7.3406" x2="181.3814" y2="7.493" layer="94"/>
<rectangle x1="182.6006" y1="7.3406" x2="182.9054" y2="7.493" layer="94"/>
<rectangle x1="183.0578" y1="7.3406" x2="183.515" y2="7.493" layer="94"/>
<rectangle x1="184.277" y1="7.3406" x2="184.5818" y2="7.493" layer="94"/>
<rectangle x1="185.039" y1="7.3406" x2="185.3438" y2="7.493" layer="94"/>
<rectangle x1="185.9534" y1="7.3406" x2="186.4106" y2="7.493" layer="94"/>
<rectangle x1="186.563" y1="7.3406" x2="187.0202" y2="7.493" layer="94"/>
<rectangle x1="187.6298" y1="7.3406" x2="188.087" y2="7.493" layer="94"/>
<rectangle x1="189.3062" y1="7.3406" x2="189.7634" y2="7.493" layer="94"/>
<rectangle x1="190.373" y1="7.3406" x2="190.5254" y2="7.493" layer="94"/>
<rectangle x1="191.135" y1="7.3406" x2="191.4398" y2="7.493" layer="94"/>
<rectangle x1="192.0494" y1="7.3406" x2="192.3542" y2="7.493" layer="94"/>
<rectangle x1="192.659" y1="7.3406" x2="193.1162" y2="7.493" layer="94"/>
<rectangle x1="193.5734" y1="7.3406" x2="193.8782" y2="7.493" layer="94"/>
<rectangle x1="194.4878" y1="7.3406" x2="194.7926" y2="7.493" layer="94"/>
<rectangle x1="195.0974" y1="7.3406" x2="195.5546" y2="7.493" layer="94"/>
<rectangle x1="196.1642" y1="7.3406" x2="196.6214" y2="7.493" layer="94"/>
<rectangle x1="196.7738" y1="7.3406" x2="197.231" y2="7.493" layer="94"/>
<rectangle x1="197.8406" y1="7.3406" x2="198.1454" y2="7.493" layer="94"/>
<rectangle x1="198.4502" y1="7.3406" x2="198.755" y2="7.493" layer="94"/>
<rectangle x1="199.517" y1="7.3406" x2="199.9742" y2="7.493" layer="94"/>
<rectangle x1="200.5838" y1="7.3406" x2="201.1934" y2="7.493" layer="94"/>
<rectangle x1="205.0034" y1="7.3406" x2="205.1558" y2="7.493" layer="94"/>
<rectangle x1="207.4418" y1="7.3406" x2="208.2038" y2="7.493" layer="94"/>
<rectangle x1="174.6758" y1="7.493" x2="174.9806" y2="7.6454" layer="94"/>
<rectangle x1="175.133" y1="7.493" x2="175.5902" y2="7.6454" layer="94"/>
<rectangle x1="176.0474" y1="7.493" x2="176.3522" y2="7.6454" layer="94"/>
<rectangle x1="176.5046" y1="7.493" x2="176.8094" y2="7.6454" layer="94"/>
<rectangle x1="177.1142" y1="7.493" x2="177.5714" y2="7.6454" layer="94"/>
<rectangle x1="178.3334" y1="7.493" x2="178.6382" y2="7.6454" layer="94"/>
<rectangle x1="178.7906" y1="7.493" x2="179.2478" y2="7.6454" layer="94"/>
<rectangle x1="180.1622" y1="7.493" x2="180.467" y2="7.6454" layer="94"/>
<rectangle x1="180.7718" y1="7.493" x2="181.229" y2="7.6454" layer="94"/>
<rectangle x1="182.6006" y1="7.493" x2="183.0578" y2="7.6454" layer="94"/>
<rectangle x1="183.2102" y1="7.493" x2="183.515" y2="7.6454" layer="94"/>
<rectangle x1="184.277" y1="7.493" x2="184.7342" y2="7.6454" layer="94"/>
<rectangle x1="184.8866" y1="7.493" x2="185.3438" y2="7.6454" layer="94"/>
<rectangle x1="185.9534" y1="7.493" x2="186.4106" y2="7.6454" layer="94"/>
<rectangle x1="186.563" y1="7.493" x2="187.0202" y2="7.6454" layer="94"/>
<rectangle x1="187.7822" y1="7.493" x2="188.087" y2="7.6454" layer="94"/>
<rectangle x1="189.1538" y1="7.493" x2="189.611" y2="7.6454" layer="94"/>
<rectangle x1="190.9826" y1="7.493" x2="191.4398" y2="7.6454" layer="94"/>
<rectangle x1="192.0494" y1="7.493" x2="192.5066" y2="7.6454" layer="94"/>
<rectangle x1="192.659" y1="7.493" x2="193.1162" y2="7.6454" layer="94"/>
<rectangle x1="193.5734" y1="7.493" x2="194.0306" y2="7.6454" layer="94"/>
<rectangle x1="194.4878" y1="7.493" x2="194.945" y2="7.6454" layer="94"/>
<rectangle x1="195.0974" y1="7.493" x2="195.4022" y2="7.6454" layer="94"/>
<rectangle x1="196.1642" y1="7.493" x2="196.6214" y2="7.6454" layer="94"/>
<rectangle x1="196.7738" y1="7.493" x2="197.0786" y2="7.6454" layer="94"/>
<rectangle x1="197.8406" y1="7.493" x2="198.2978" y2="7.6454" layer="94"/>
<rectangle x1="198.4502" y1="7.493" x2="198.9074" y2="7.6454" layer="94"/>
<rectangle x1="199.517" y1="7.493" x2="199.9742" y2="7.6454" layer="94"/>
<rectangle x1="200.4314" y1="7.493" x2="201.3458" y2="7.6454" layer="94"/>
<rectangle x1="205.0034" y1="7.493" x2="205.1558" y2="7.6454" layer="94"/>
<rectangle x1="207.899" y1="7.493" x2="208.3562" y2="7.6454" layer="94"/>
<rectangle x1="174.6758" y1="7.6454" x2="175.133" y2="7.7978" layer="94"/>
<rectangle x1="175.2854" y1="7.6454" x2="175.5902" y2="7.7978" layer="94"/>
<rectangle x1="176.0474" y1="7.6454" x2="176.3522" y2="7.7978" layer="94"/>
<rectangle x1="176.5046" y1="7.6454" x2="176.9618" y2="7.7978" layer="94"/>
<rectangle x1="177.2666" y1="7.6454" x2="177.5714" y2="7.7978" layer="94"/>
<rectangle x1="178.3334" y1="7.6454" x2="178.6382" y2="7.7978" layer="94"/>
<rectangle x1="178.943" y1="7.6454" x2="179.2478" y2="7.7978" layer="94"/>
<rectangle x1="180.1622" y1="7.6454" x2="181.229" y2="7.7978" layer="94"/>
<rectangle x1="182.1434" y1="7.6454" x2="182.9054" y2="7.7978" layer="94"/>
<rectangle x1="183.2102" y1="7.6454" x2="183.515" y2="7.7978" layer="94"/>
<rectangle x1="184.277" y1="7.6454" x2="184.7342" y2="7.7978" layer="94"/>
<rectangle x1="184.8866" y1="7.6454" x2="185.3438" y2="7.7978" layer="94"/>
<rectangle x1="186.1058" y1="7.6454" x2="186.4106" y2="7.7978" layer="94"/>
<rectangle x1="186.563" y1="7.6454" x2="187.0202" y2="7.7978" layer="94"/>
<rectangle x1="187.7822" y1="7.6454" x2="188.087" y2="7.7978" layer="94"/>
<rectangle x1="189.1538" y1="7.6454" x2="189.611" y2="7.7978" layer="94"/>
<rectangle x1="190.9826" y1="7.6454" x2="191.4398" y2="7.7978" layer="94"/>
<rectangle x1="192.2018" y1="7.6454" x2="192.5066" y2="7.7978" layer="94"/>
<rectangle x1="192.8114" y1="7.6454" x2="193.1162" y2="7.7978" layer="94"/>
<rectangle x1="193.5734" y1="7.6454" x2="194.0306" y2="7.7978" layer="94"/>
<rectangle x1="194.4878" y1="7.6454" x2="194.7926" y2="7.7978" layer="94"/>
<rectangle x1="195.0974" y1="7.6454" x2="195.5546" y2="7.7978" layer="94"/>
<rectangle x1="196.3166" y1="7.6454" x2="196.6214" y2="7.7978" layer="94"/>
<rectangle x1="196.7738" y1="7.6454" x2="197.231" y2="7.7978" layer="94"/>
<rectangle x1="197.8406" y1="7.6454" x2="198.2978" y2="7.7978" layer="94"/>
<rectangle x1="198.4502" y1="7.6454" x2="198.9074" y2="7.7978" layer="94"/>
<rectangle x1="199.6694" y1="7.6454" x2="199.9742" y2="7.7978" layer="94"/>
<rectangle x1="200.4314" y1="7.6454" x2="201.3458" y2="7.7978" layer="94"/>
<rectangle x1="205.0034" y1="7.6454" x2="205.3082" y2="7.7978" layer="94"/>
<rectangle x1="208.0514" y1="7.6454" x2="208.3562" y2="7.7978" layer="94"/>
<rectangle x1="174.6758" y1="7.7978" x2="174.9806" y2="7.9502" layer="94"/>
<rectangle x1="175.2854" y1="7.7978" x2="175.5902" y2="7.9502" layer="94"/>
<rectangle x1="176.0474" y1="7.7978" x2="176.3522" y2="7.9502" layer="94"/>
<rectangle x1="176.5046" y1="7.7978" x2="176.9618" y2="7.9502" layer="94"/>
<rectangle x1="177.1142" y1="7.7978" x2="177.5714" y2="7.9502" layer="94"/>
<rectangle x1="178.3334" y1="7.7978" x2="178.7906" y2="7.9502" layer="94"/>
<rectangle x1="178.943" y1="7.7978" x2="179.2478" y2="7.9502" layer="94"/>
<rectangle x1="180.1622" y1="7.7978" x2="181.0766" y2="7.9502" layer="94"/>
<rectangle x1="181.8386" y1="7.7978" x2="182.753" y2="7.9502" layer="94"/>
<rectangle x1="183.2102" y1="7.7978" x2="183.6674" y2="7.9502" layer="94"/>
<rectangle x1="184.277" y1="7.7978" x2="184.7342" y2="7.9502" layer="94"/>
<rectangle x1="184.8866" y1="7.7978" x2="185.3438" y2="7.9502" layer="94"/>
<rectangle x1="186.1058" y1="7.7978" x2="186.4106" y2="7.9502" layer="94"/>
<rectangle x1="186.7154" y1="7.7978" x2="187.0202" y2="7.9502" layer="94"/>
<rectangle x1="187.7822" y1="7.7978" x2="188.2394" y2="7.9502" layer="94"/>
<rectangle x1="189.1538" y1="7.7978" x2="189.611" y2="7.9502" layer="94"/>
<rectangle x1="190.9826" y1="7.7978" x2="191.4398" y2="7.9502" layer="94"/>
<rectangle x1="192.2018" y1="7.7978" x2="192.5066" y2="7.9502" layer="94"/>
<rectangle x1="192.8114" y1="7.7978" x2="193.1162" y2="7.9502" layer="94"/>
<rectangle x1="193.5734" y1="7.7978" x2="194.0306" y2="7.9502" layer="94"/>
<rectangle x1="194.4878" y1="7.7978" x2="194.945" y2="7.9502" layer="94"/>
<rectangle x1="195.0974" y1="7.7978" x2="195.5546" y2="7.9502" layer="94"/>
<rectangle x1="196.3166" y1="7.7978" x2="196.6214" y2="7.9502" layer="94"/>
<rectangle x1="196.7738" y1="7.7978" x2="198.2978" y2="7.9502" layer="94"/>
<rectangle x1="198.6026" y1="7.7978" x2="198.9074" y2="7.9502" layer="94"/>
<rectangle x1="199.6694" y1="7.7978" x2="199.9742" y2="7.9502" layer="94"/>
<rectangle x1="200.4314" y1="7.7978" x2="200.7362" y2="7.9502" layer="94"/>
<rectangle x1="201.041" y1="7.7978" x2="201.4982" y2="7.9502" layer="94"/>
<rectangle x1="205.1558" y1="7.7978" x2="205.4606" y2="7.9502" layer="94"/>
<rectangle x1="208.0514" y1="7.7978" x2="208.3562" y2="7.9502" layer="94"/>
<rectangle x1="174.6758" y1="7.9502" x2="175.133" y2="8.1026" layer="94"/>
<rectangle x1="175.2854" y1="7.9502" x2="175.7426" y2="8.1026" layer="94"/>
<rectangle x1="176.0474" y1="7.9502" x2="176.3522" y2="8.1026" layer="94"/>
<rectangle x1="176.657" y1="7.9502" x2="176.9618" y2="8.1026" layer="94"/>
<rectangle x1="177.2666" y1="7.9502" x2="177.5714" y2="8.1026" layer="94"/>
<rectangle x1="178.3334" y1="7.9502" x2="178.6382" y2="8.1026" layer="94"/>
<rectangle x1="178.943" y1="7.9502" x2="179.4002" y2="8.1026" layer="94"/>
<rectangle x1="180.1622" y1="7.9502" x2="181.229" y2="8.1026" layer="94"/>
<rectangle x1="181.8386" y1="7.9502" x2="182.4482" y2="8.1026" layer="94"/>
<rectangle x1="183.2102" y1="7.9502" x2="183.6674" y2="8.1026" layer="94"/>
<rectangle x1="184.4294" y1="7.9502" x2="184.7342" y2="8.1026" layer="94"/>
<rectangle x1="185.039" y1="7.9502" x2="185.3438" y2="8.1026" layer="94"/>
<rectangle x1="186.1058" y1="7.9502" x2="186.4106" y2="8.1026" layer="94"/>
<rectangle x1="186.7154" y1="7.9502" x2="187.0202" y2="8.1026" layer="94"/>
<rectangle x1="187.7822" y1="7.9502" x2="188.2394" y2="8.1026" layer="94"/>
<rectangle x1="189.1538" y1="7.9502" x2="189.611" y2="8.1026" layer="94"/>
<rectangle x1="191.135" y1="7.9502" x2="191.4398" y2="8.1026" layer="94"/>
<rectangle x1="192.2018" y1="7.9502" x2="192.5066" y2="8.1026" layer="94"/>
<rectangle x1="192.8114" y1="7.9502" x2="193.1162" y2="8.1026" layer="94"/>
<rectangle x1="193.7258" y1="7.9502" x2="194.0306" y2="8.1026" layer="94"/>
<rectangle x1="194.4878" y1="7.9502" x2="194.945" y2="8.1026" layer="94"/>
<rectangle x1="195.2498" y1="7.9502" x2="195.5546" y2="8.1026" layer="94"/>
<rectangle x1="196.3166" y1="7.9502" x2="196.6214" y2="8.1026" layer="94"/>
<rectangle x1="197.0786" y1="7.9502" x2="198.2978" y2="8.1026" layer="94"/>
<rectangle x1="198.6026" y1="7.9502" x2="198.9074" y2="8.1026" layer="94"/>
<rectangle x1="199.6694" y1="7.9502" x2="200.1266" y2="8.1026" layer="94"/>
<rectangle x1="200.279" y1="7.9502" x2="200.7362" y2="8.1026" layer="94"/>
<rectangle x1="201.041" y1="7.9502" x2="201.4982" y2="8.1026" layer="94"/>
<rectangle x1="205.0034" y1="7.9502" x2="205.3082" y2="8.1026" layer="94"/>
<rectangle x1="206.5274" y1="7.9502" x2="206.6798" y2="8.1026" layer="94"/>
<rectangle x1="206.8322" y1="7.9502" x2="206.9846" y2="8.1026" layer="94"/>
<rectangle x1="207.5942" y1="7.9502" x2="208.2038" y2="8.1026" layer="94"/>
<rectangle x1="174.6758" y1="8.1026" x2="174.9806" y2="8.255" layer="94"/>
<rectangle x1="175.2854" y1="8.1026" x2="175.7426" y2="8.255" layer="94"/>
<rectangle x1="176.0474" y1="8.1026" x2="176.3522" y2="8.255" layer="94"/>
<rectangle x1="176.657" y1="8.1026" x2="177.1142" y2="8.255" layer="94"/>
<rectangle x1="177.2666" y1="8.1026" x2="177.7238" y2="8.255" layer="94"/>
<rectangle x1="178.3334" y1="8.1026" x2="178.7906" y2="8.255" layer="94"/>
<rectangle x1="178.943" y1="8.1026" x2="179.4002" y2="8.255" layer="94"/>
<rectangle x1="180.3146" y1="8.1026" x2="180.6194" y2="8.255" layer="94"/>
<rectangle x1="180.9242" y1="8.1026" x2="181.3814" y2="8.255" layer="94"/>
<rectangle x1="181.8386" y1="8.1026" x2="182.1434" y2="8.255" layer="94"/>
<rectangle x1="183.2102" y1="8.1026" x2="183.6674" y2="8.255" layer="94"/>
<rectangle x1="184.277" y1="8.1026" x2="184.7342" y2="8.255" layer="94"/>
<rectangle x1="185.039" y1="8.1026" x2="185.3438" y2="8.255" layer="94"/>
<rectangle x1="186.1058" y1="8.1026" x2="186.4106" y2="8.255" layer="94"/>
<rectangle x1="186.7154" y1="8.1026" x2="187.1726" y2="8.255" layer="94"/>
<rectangle x1="187.7822" y1="8.1026" x2="188.2394" y2="8.255" layer="94"/>
<rectangle x1="189.3062" y1="8.1026" x2="189.611" y2="8.255" layer="94"/>
<rectangle x1="191.135" y1="8.1026" x2="191.5922" y2="8.255" layer="94"/>
<rectangle x1="192.2018" y1="8.1026" x2="192.5066" y2="8.255" layer="94"/>
<rectangle x1="192.8114" y1="8.1026" x2="193.2686" y2="8.255" layer="94"/>
<rectangle x1="193.7258" y1="8.1026" x2="194.0306" y2="8.255" layer="94"/>
<rectangle x1="194.6402" y1="8.1026" x2="194.945" y2="8.255" layer="94"/>
<rectangle x1="195.2498" y1="8.1026" x2="195.5546" y2="8.255" layer="94"/>
<rectangle x1="196.3166" y1="8.1026" x2="196.6214" y2="8.255" layer="94"/>
<rectangle x1="197.993" y1="8.1026" x2="198.2978" y2="8.255" layer="94"/>
<rectangle x1="198.6026" y1="8.1026" x2="199.0598" y2="8.255" layer="94"/>
<rectangle x1="199.6694" y1="8.1026" x2="199.9742" y2="8.255" layer="94"/>
<rectangle x1="200.279" y1="8.1026" x2="200.7362" y2="8.255" layer="94"/>
<rectangle x1="201.1934" y1="8.1026" x2="201.6506" y2="8.255" layer="94"/>
<rectangle x1="205.0034" y1="8.1026" x2="205.1558" y2="8.255" layer="94"/>
<rectangle x1="207.2894" y1="8.1026" x2="207.4418" y2="8.255" layer="94"/>
<rectangle x1="207.5942" y1="8.1026" x2="208.2038" y2="8.255" layer="94"/>
<rectangle x1="174.6758" y1="8.255" x2="175.133" y2="8.4074" layer="94"/>
<rectangle x1="175.4378" y1="8.255" x2="175.7426" y2="8.4074" layer="94"/>
<rectangle x1="176.0474" y1="8.255" x2="176.3522" y2="8.4074" layer="94"/>
<rectangle x1="176.8094" y1="8.255" x2="177.1142" y2="8.4074" layer="94"/>
<rectangle x1="177.419" y1="8.255" x2="177.8762" y2="8.4074" layer="94"/>
<rectangle x1="178.0286" y1="8.255" x2="178.6382" y2="8.4074" layer="94"/>
<rectangle x1="179.0954" y1="8.255" x2="179.5526" y2="8.4074" layer="94"/>
<rectangle x1="179.8574" y1="8.255" x2="180.1622" y2="8.4074" layer="94"/>
<rectangle x1="180.3146" y1="8.255" x2="180.6194" y2="8.4074" layer="94"/>
<rectangle x1="181.0766" y1="8.255" x2="181.6862" y2="8.4074" layer="94"/>
<rectangle x1="181.8386" y1="8.255" x2="182.2958" y2="8.4074" layer="94"/>
<rectangle x1="182.6006" y1="8.255" x2="182.9054" y2="8.4074" layer="94"/>
<rectangle x1="183.3626" y1="8.255" x2="183.8198" y2="8.4074" layer="94"/>
<rectangle x1="183.9722" y1="8.255" x2="184.1246" y2="8.4074" layer="94"/>
<rectangle x1="184.277" y1="8.255" x2="184.7342" y2="8.4074" layer="94"/>
<rectangle x1="185.039" y1="8.255" x2="185.6486" y2="8.4074" layer="94"/>
<rectangle x1="185.801" y1="8.255" x2="186.4106" y2="8.4074" layer="94"/>
<rectangle x1="186.8678" y1="8.255" x2="187.325" y2="8.4074" layer="94"/>
<rectangle x1="187.4774" y1="8.255" x2="188.087" y2="8.4074" layer="94"/>
<rectangle x1="189.3062" y1="8.255" x2="189.611" y2="8.4074" layer="94"/>
<rectangle x1="191.135" y1="8.255" x2="191.7446" y2="8.4074" layer="94"/>
<rectangle x1="191.897" y1="8.255" x2="192.5066" y2="8.4074" layer="94"/>
<rectangle x1="192.9638" y1="8.255" x2="193.421" y2="8.4074" layer="94"/>
<rectangle x1="193.5734" y1="8.255" x2="194.183" y2="8.4074" layer="94"/>
<rectangle x1="194.3354" y1="8.255" x2="194.945" y2="8.4074" layer="94"/>
<rectangle x1="195.2498" y1="8.255" x2="195.8594" y2="8.4074" layer="94"/>
<rectangle x1="196.0118" y1="8.255" x2="196.6214" y2="8.4074" layer="94"/>
<rectangle x1="197.0786" y1="8.255" x2="197.3834" y2="8.4074" layer="94"/>
<rectangle x1="197.8406" y1="8.255" x2="198.2978" y2="8.4074" layer="94"/>
<rectangle x1="198.755" y1="8.255" x2="199.2122" y2="8.4074" layer="94"/>
<rectangle x1="199.3646" y1="8.255" x2="199.9742" y2="8.4074" layer="94"/>
<rectangle x1="200.279" y1="8.255" x2="200.7362" y2="8.4074" layer="94"/>
<rectangle x1="201.3458" y1="8.255" x2="201.6506" y2="8.4074" layer="94"/>
<rectangle x1="204.851" y1="8.255" x2="205.1558" y2="8.4074" layer="94"/>
<rectangle x1="208.0514" y1="8.255" x2="208.3562" y2="8.4074" layer="94"/>
<rectangle x1="174.6758" y1="8.4074" x2="174.9806" y2="8.5598" layer="94"/>
<rectangle x1="175.4378" y1="8.4074" x2="175.895" y2="8.5598" layer="94"/>
<rectangle x1="176.0474" y1="8.4074" x2="176.3522" y2="8.5598" layer="94"/>
<rectangle x1="176.8094" y1="8.4074" x2="177.2666" y2="8.5598" layer="94"/>
<rectangle x1="177.419" y1="8.4074" x2="178.6382" y2="8.5598" layer="94"/>
<rectangle x1="179.2478" y1="8.4074" x2="180.6194" y2="8.5598" layer="94"/>
<rectangle x1="181.229" y1="8.4074" x2="181.6862" y2="8.5598" layer="94"/>
<rectangle x1="181.8386" y1="8.4074" x2="183.0578" y2="8.5598" layer="94"/>
<rectangle x1="183.3626" y1="8.4074" x2="184.5818" y2="8.5598" layer="94"/>
<rectangle x1="185.1914" y1="8.4074" x2="186.2582" y2="8.5598" layer="94"/>
<rectangle x1="187.0202" y1="8.4074" x2="188.087" y2="8.5598" layer="94"/>
<rectangle x1="189.3062" y1="8.4074" x2="189.7634" y2="8.5598" layer="94"/>
<rectangle x1="191.2874" y1="8.4074" x2="192.3542" y2="8.5598" layer="94"/>
<rectangle x1="192.9638" y1="8.4074" x2="194.7926" y2="8.5598" layer="94"/>
<rectangle x1="195.4022" y1="8.4074" x2="196.469" y2="8.5598" layer="94"/>
<rectangle x1="197.0786" y1="8.4074" x2="198.1454" y2="8.5598" layer="94"/>
<rectangle x1="198.755" y1="8.4074" x2="199.8218" y2="8.5598" layer="94"/>
<rectangle x1="200.279" y1="8.4074" x2="200.5838" y2="8.5598" layer="94"/>
<rectangle x1="201.3458" y1="8.4074" x2="201.803" y2="8.5598" layer="94"/>
<rectangle x1="205.0034" y1="8.4074" x2="205.3082" y2="8.5598" layer="94"/>
<rectangle x1="208.0514" y1="8.4074" x2="208.3562" y2="8.5598" layer="94"/>
<rectangle x1="174.6758" y1="8.5598" x2="175.133" y2="8.7122" layer="94"/>
<rectangle x1="175.5902" y1="8.5598" x2="175.895" y2="8.7122" layer="94"/>
<rectangle x1="176.0474" y1="8.5598" x2="176.3522" y2="8.7122" layer="94"/>
<rectangle x1="176.8094" y1="8.5598" x2="177.2666" y2="8.7122" layer="94"/>
<rectangle x1="177.7238" y1="8.5598" x2="178.181" y2="8.7122" layer="94"/>
<rectangle x1="179.4002" y1="8.5598" x2="180.0098" y2="8.7122" layer="94"/>
<rectangle x1="180.3146" y1="8.5598" x2="180.7718" y2="8.7122" layer="94"/>
<rectangle x1="181.3814" y1="8.5598" x2="181.5338" y2="8.7122" layer="94"/>
<rectangle x1="182.1434" y1="8.5598" x2="182.753" y2="8.7122" layer="94"/>
<rectangle x1="183.3626" y1="8.5598" x2="183.8198" y2="8.7122" layer="94"/>
<rectangle x1="183.9722" y1="8.5598" x2="184.277" y2="8.7122" layer="94"/>
<rectangle x1="185.4962" y1="8.5598" x2="185.9534" y2="8.7122" layer="94"/>
<rectangle x1="187.1726" y1="8.5598" x2="187.6298" y2="8.7122" layer="94"/>
<rectangle x1="189.3062" y1="8.5598" x2="189.7634" y2="8.7122" layer="94"/>
<rectangle x1="191.5922" y1="8.5598" x2="192.0494" y2="8.7122" layer="94"/>
<rectangle x1="193.2686" y1="8.5598" x2="193.7258" y2="8.7122" layer="94"/>
<rectangle x1="194.183" y1="8.5598" x2="194.4878" y2="8.7122" layer="94"/>
<rectangle x1="195.707" y1="8.5598" x2="196.1642" y2="8.7122" layer="94"/>
<rectangle x1="197.231" y1="8.5598" x2="197.8406" y2="8.7122" layer="94"/>
<rectangle x1="199.0598" y1="8.5598" x2="199.517" y2="8.7122" layer="94"/>
<rectangle x1="200.279" y1="8.5598" x2="200.4314" y2="8.7122" layer="94"/>
<rectangle x1="201.4982" y1="8.5598" x2="201.6506" y2="8.7122" layer="94"/>
<rectangle x1="205.0034" y1="8.5598" x2="205.4606" y2="8.7122" layer="94"/>
<rectangle x1="208.0514" y1="8.5598" x2="208.3562" y2="8.7122" layer="94"/>
<rectangle x1="174.6758" y1="8.7122" x2="174.9806" y2="8.8646" layer="94"/>
<rectangle x1="175.5902" y1="8.7122" x2="176.3522" y2="8.8646" layer="94"/>
<rectangle x1="176.9618" y1="8.7122" x2="177.2666" y2="8.8646" layer="94"/>
<rectangle x1="180.3146" y1="8.7122" x2="180.7718" y2="8.8646" layer="94"/>
<rectangle x1="183.3626" y1="8.7122" x2="183.8198" y2="8.8646" layer="94"/>
<rectangle x1="189.4586" y1="8.7122" x2="189.9158" y2="8.8646" layer="94"/>
<rectangle x1="190.6778" y1="8.7122" x2="190.8302" y2="8.8646" layer="94"/>
<rectangle x1="205.0034" y1="8.7122" x2="205.1558" y2="8.8646" layer="94"/>
<rectangle x1="207.137" y1="8.7122" x2="208.2038" y2="8.8646" layer="94"/>
<rectangle x1="174.6758" y1="8.8646" x2="175.133" y2="9.017" layer="94"/>
<rectangle x1="175.5902" y1="8.8646" x2="176.3522" y2="9.017" layer="94"/>
<rectangle x1="176.9618" y1="8.8646" x2="177.2666" y2="9.017" layer="94"/>
<rectangle x1="180.3146" y1="8.8646" x2="180.7718" y2="9.017" layer="94"/>
<rectangle x1="183.3626" y1="8.8646" x2="183.8198" y2="9.017" layer="94"/>
<rectangle x1="189.4586" y1="8.8646" x2="190.9826" y2="9.017" layer="94"/>
<rectangle x1="204.851" y1="8.8646" x2="205.1558" y2="9.017" layer="94"/>
<rectangle x1="207.5942" y1="8.8646" x2="208.2038" y2="9.017" layer="94"/>
<rectangle x1="174.6758" y1="9.017" x2="174.9806" y2="9.1694" layer="94"/>
<rectangle x1="175.7426" y1="9.017" x2="176.3522" y2="9.1694" layer="94"/>
<rectangle x1="176.9618" y1="9.017" x2="177.419" y2="9.1694" layer="94"/>
<rectangle x1="180.467" y1="9.017" x2="180.7718" y2="9.1694" layer="94"/>
<rectangle x1="183.515" y1="9.017" x2="183.8198" y2="9.1694" layer="94"/>
<rectangle x1="189.7634" y1="9.017" x2="190.8302" y2="9.1694" layer="94"/>
<rectangle x1="205.0034" y1="9.017" x2="205.1558" y2="9.1694" layer="94"/>
<rectangle x1="207.899" y1="9.017" x2="208.3562" y2="9.1694" layer="94"/>
<rectangle x1="180.467" y1="9.1694" x2="180.7718" y2="9.3218" layer="94"/>
<rectangle x1="183.515" y1="9.1694" x2="183.8198" y2="9.3218" layer="94"/>
<rectangle x1="205.1558" y1="9.1694" x2="205.3082" y2="9.3218" layer="94"/>
<rectangle x1="207.899" y1="9.1694" x2="208.3562" y2="9.3218" layer="94"/>
<rectangle x1="207.7466" y1="9.3218" x2="208.3562" y2="9.4742" layer="94"/>
<rectangle x1="207.2894" y1="9.4742" x2="207.4418" y2="9.6266" layer="94"/>
<rectangle x1="207.5942" y1="9.4742" x2="207.899" y2="9.6266" layer="94"/>
<rectangle x1="205.613" y1="9.9314" x2="207.4418" y2="10.0838" layer="94"/>
<rectangle x1="174.8282" y1="10.0838" x2="175.895" y2="10.2362" layer="94"/>
<rectangle x1="180.0098" y1="10.0838" x2="180.9242" y2="10.2362" layer="94"/>
<rectangle x1="182.6006" y1="10.0838" x2="183.515" y2="10.2362" layer="94"/>
<rectangle x1="187.6298" y1="10.0838" x2="188.3918" y2="10.2362" layer="94"/>
<rectangle x1="192.2018" y1="10.0838" x2="194.3354" y2="10.2362" layer="94"/>
<rectangle x1="199.8218" y1="10.0838" x2="200.8886" y2="10.2362" layer="94"/>
<rectangle x1="205.1558" y1="10.0838" x2="207.899" y2="10.2362" layer="94"/>
<rectangle x1="174.5234" y1="10.2362" x2="176.1998" y2="10.3886" layer="94"/>
<rectangle x1="179.8574" y1="10.2362" x2="181.0766" y2="10.3886" layer="94"/>
<rectangle x1="182.2958" y1="10.2362" x2="183.6674" y2="10.3886" layer="94"/>
<rectangle x1="187.325" y1="10.2362" x2="188.6966" y2="10.3886" layer="94"/>
<rectangle x1="191.7446" y1="10.2362" x2="194.945" y2="10.3886" layer="94"/>
<rectangle x1="199.517" y1="10.2362" x2="201.041" y2="10.3886" layer="94"/>
<rectangle x1="204.851" y1="10.2362" x2="208.3562" y2="10.3886" layer="94"/>
<rectangle x1="174.371" y1="10.3886" x2="176.1998" y2="10.541" layer="94"/>
<rectangle x1="179.705" y1="10.3886" x2="181.229" y2="10.541" layer="94"/>
<rectangle x1="182.1434" y1="10.3886" x2="183.8198" y2="10.541" layer="94"/>
<rectangle x1="187.1726" y1="10.3886" x2="188.849" y2="10.541" layer="94"/>
<rectangle x1="191.4398" y1="10.3886" x2="195.2498" y2="10.541" layer="94"/>
<rectangle x1="199.3646" y1="10.3886" x2="201.1934" y2="10.541" layer="94"/>
<rectangle x1="204.5462" y1="10.3886" x2="208.5086" y2="10.541" layer="94"/>
<rectangle x1="174.371" y1="10.541" x2="176.3522" y2="10.6934" layer="94"/>
<rectangle x1="179.5526" y1="10.541" x2="181.3814" y2="10.6934" layer="94"/>
<rectangle x1="182.1434" y1="10.541" x2="183.8198" y2="10.6934" layer="94"/>
<rectangle x1="187.1726" y1="10.541" x2="188.849" y2="10.6934" layer="94"/>
<rectangle x1="191.135" y1="10.541" x2="195.4022" y2="10.6934" layer="94"/>
<rectangle x1="199.3646" y1="10.541" x2="201.3458" y2="10.6934" layer="94"/>
<rectangle x1="204.2414" y1="10.541" x2="208.8134" y2="10.6934" layer="94"/>
<rectangle x1="174.371" y1="10.6934" x2="176.3522" y2="10.8458" layer="94"/>
<rectangle x1="179.5526" y1="10.6934" x2="181.3814" y2="10.8458" layer="94"/>
<rectangle x1="182.1434" y1="10.6934" x2="183.9722" y2="10.8458" layer="94"/>
<rectangle x1="187.1726" y1="10.6934" x2="188.849" y2="10.8458" layer="94"/>
<rectangle x1="190.9826" y1="10.6934" x2="195.707" y2="10.8458" layer="94"/>
<rectangle x1="199.2122" y1="10.6934" x2="201.3458" y2="10.8458" layer="94"/>
<rectangle x1="204.089" y1="10.6934" x2="208.9658" y2="10.8458" layer="94"/>
<rectangle x1="174.2186" y1="10.8458" x2="176.3522" y2="10.9982" layer="94"/>
<rectangle x1="179.5526" y1="10.8458" x2="181.3814" y2="10.9982" layer="94"/>
<rectangle x1="182.1434" y1="10.8458" x2="183.9722" y2="10.9982" layer="94"/>
<rectangle x1="187.0202" y1="10.8458" x2="189.0014" y2="10.9982" layer="94"/>
<rectangle x1="190.6778" y1="10.8458" x2="195.8594" y2="10.9982" layer="94"/>
<rectangle x1="199.2122" y1="10.8458" x2="201.4982" y2="10.9982" layer="94"/>
<rectangle x1="203.9366" y1="10.8458" x2="209.2706" y2="10.9982" layer="94"/>
<rectangle x1="174.2186" y1="10.9982" x2="176.3522" y2="11.1506" layer="94"/>
<rectangle x1="179.5526" y1="10.9982" x2="181.3814" y2="11.1506" layer="94"/>
<rectangle x1="181.991" y1="10.9982" x2="183.9722" y2="11.1506" layer="94"/>
<rectangle x1="187.0202" y1="10.9982" x2="189.0014" y2="11.1506" layer="94"/>
<rectangle x1="190.5254" y1="10.9982" x2="196.0118" y2="11.1506" layer="94"/>
<rectangle x1="199.0598" y1="10.9982" x2="201.4982" y2="11.1506" layer="94"/>
<rectangle x1="203.7842" y1="10.9982" x2="209.423" y2="11.1506" layer="94"/>
<rectangle x1="174.2186" y1="11.1506" x2="176.3522" y2="11.303" layer="94"/>
<rectangle x1="179.4002" y1="11.1506" x2="181.3814" y2="11.303" layer="94"/>
<rectangle x1="181.991" y1="11.1506" x2="183.9722" y2="11.303" layer="94"/>
<rectangle x1="187.0202" y1="11.1506" x2="189.0014" y2="11.303" layer="94"/>
<rectangle x1="190.373" y1="11.1506" x2="196.1642" y2="11.303" layer="94"/>
<rectangle x1="199.0598" y1="11.1506" x2="201.6506" y2="11.303" layer="94"/>
<rectangle x1="203.4794" y1="11.1506" x2="209.423" y2="11.303" layer="94"/>
<rectangle x1="174.2186" y1="11.303" x2="176.5046" y2="11.4554" layer="94"/>
<rectangle x1="179.5526" y1="11.303" x2="181.3814" y2="11.4554" layer="94"/>
<rectangle x1="181.991" y1="11.303" x2="183.9722" y2="11.4554" layer="94"/>
<rectangle x1="187.0202" y1="11.303" x2="189.0014" y2="11.4554" layer="94"/>
<rectangle x1="190.2206" y1="11.303" x2="196.3166" y2="11.4554" layer="94"/>
<rectangle x1="198.9074" y1="11.303" x2="201.6506" y2="11.4554" layer="94"/>
<rectangle x1="203.4794" y1="11.303" x2="206.2226" y2="11.4554" layer="94"/>
<rectangle x1="206.375" y1="11.303" x2="206.5274" y2="11.4554" layer="94"/>
<rectangle x1="206.6798" y1="11.303" x2="206.8322" y2="11.4554" layer="94"/>
<rectangle x1="206.9846" y1="11.303" x2="209.2706" y2="11.4554" layer="94"/>
<rectangle x1="174.2186" y1="11.4554" x2="176.3522" y2="11.6078" layer="94"/>
<rectangle x1="179.4002" y1="11.4554" x2="181.3814" y2="11.6078" layer="94"/>
<rectangle x1="181.991" y1="11.4554" x2="183.9722" y2="11.6078" layer="94"/>
<rectangle x1="187.0202" y1="11.4554" x2="189.0014" y2="11.6078" layer="94"/>
<rectangle x1="190.2206" y1="11.4554" x2="192.8114" y2="11.6078" layer="94"/>
<rectangle x1="193.5734" y1="11.4554" x2="196.469" y2="11.6078" layer="94"/>
<rectangle x1="198.9074" y1="11.4554" x2="201.6506" y2="11.6078" layer="94"/>
<rectangle x1="203.327" y1="11.4554" x2="205.613" y2="11.6078" layer="94"/>
<rectangle x1="207.4418" y1="11.4554" x2="209.1182" y2="11.6078" layer="94"/>
<rectangle x1="174.2186" y1="11.6078" x2="176.5046" y2="11.7602" layer="94"/>
<rectangle x1="179.5526" y1="11.6078" x2="181.3814" y2="11.7602" layer="94"/>
<rectangle x1="181.991" y1="11.6078" x2="183.9722" y2="11.7602" layer="94"/>
<rectangle x1="187.0202" y1="11.6078" x2="189.0014" y2="11.7602" layer="94"/>
<rectangle x1="190.0682" y1="11.6078" x2="192.5066" y2="11.7602" layer="94"/>
<rectangle x1="194.183" y1="11.6078" x2="196.6214" y2="11.7602" layer="94"/>
<rectangle x1="198.755" y1="11.6078" x2="201.803" y2="11.7602" layer="94"/>
<rectangle x1="203.1746" y1="11.6078" x2="205.3082" y2="11.7602" layer="94"/>
<rectangle x1="207.7466" y1="11.6078" x2="208.9658" y2="11.7602" layer="94"/>
<rectangle x1="174.2186" y1="11.7602" x2="176.3522" y2="11.9126" layer="94"/>
<rectangle x1="179.4002" y1="11.7602" x2="181.3814" y2="11.9126" layer="94"/>
<rectangle x1="181.991" y1="11.7602" x2="183.9722" y2="11.9126" layer="94"/>
<rectangle x1="187.0202" y1="11.7602" x2="189.0014" y2="11.9126" layer="94"/>
<rectangle x1="189.9158" y1="11.7602" x2="192.2018" y2="11.9126" layer="94"/>
<rectangle x1="194.3354" y1="11.7602" x2="196.6214" y2="11.9126" layer="94"/>
<rectangle x1="198.755" y1="11.7602" x2="201.803" y2="11.9126" layer="94"/>
<rectangle x1="203.1746" y1="11.7602" x2="205.0034" y2="11.9126" layer="94"/>
<rectangle x1="208.0514" y1="11.7602" x2="208.8134" y2="11.9126" layer="94"/>
<rectangle x1="174.2186" y1="11.9126" x2="176.5046" y2="12.065" layer="94"/>
<rectangle x1="179.5526" y1="11.9126" x2="181.3814" y2="12.065" layer="94"/>
<rectangle x1="181.991" y1="11.9126" x2="183.9722" y2="12.065" layer="94"/>
<rectangle x1="187.0202" y1="11.9126" x2="189.0014" y2="12.065" layer="94"/>
<rectangle x1="189.9158" y1="11.9126" x2="192.0494" y2="12.065" layer="94"/>
<rectangle x1="194.4878" y1="11.9126" x2="196.7738" y2="12.065" layer="94"/>
<rectangle x1="198.755" y1="11.9126" x2="201.9554" y2="12.065" layer="94"/>
<rectangle x1="203.0222" y1="11.9126" x2="204.851" y2="12.065" layer="94"/>
<rectangle x1="208.2038" y1="11.9126" x2="208.661" y2="12.065" layer="94"/>
<rectangle x1="174.2186" y1="12.065" x2="176.3522" y2="12.2174" layer="94"/>
<rectangle x1="179.4002" y1="12.065" x2="181.3814" y2="12.2174" layer="94"/>
<rectangle x1="181.991" y1="12.065" x2="183.9722" y2="12.2174" layer="94"/>
<rectangle x1="187.0202" y1="12.065" x2="189.0014" y2="12.2174" layer="94"/>
<rectangle x1="189.7634" y1="12.065" x2="191.897" y2="12.2174" layer="94"/>
<rectangle x1="194.6402" y1="12.065" x2="196.7738" y2="12.2174" layer="94"/>
<rectangle x1="198.6026" y1="12.065" x2="201.9554" y2="12.2174" layer="94"/>
<rectangle x1="202.8698" y1="12.065" x2="204.6986" y2="12.2174" layer="94"/>
<rectangle x1="174.2186" y1="12.2174" x2="176.5046" y2="12.3698" layer="94"/>
<rectangle x1="179.5526" y1="12.2174" x2="181.3814" y2="12.3698" layer="94"/>
<rectangle x1="181.991" y1="12.2174" x2="183.9722" y2="12.3698" layer="94"/>
<rectangle x1="187.0202" y1="12.2174" x2="189.0014" y2="12.3698" layer="94"/>
<rectangle x1="189.7634" y1="12.2174" x2="191.7446" y2="12.3698" layer="94"/>
<rectangle x1="194.7926" y1="12.2174" x2="196.9262" y2="12.3698" layer="94"/>
<rectangle x1="198.6026" y1="12.2174" x2="202.1078" y2="12.3698" layer="94"/>
<rectangle x1="202.8698" y1="12.2174" x2="204.5462" y2="12.3698" layer="94"/>
<rectangle x1="174.2186" y1="12.3698" x2="176.3522" y2="12.5222" layer="94"/>
<rectangle x1="179.4002" y1="12.3698" x2="181.3814" y2="12.5222" layer="94"/>
<rectangle x1="181.991" y1="12.3698" x2="183.9722" y2="12.5222" layer="94"/>
<rectangle x1="187.0202" y1="12.3698" x2="189.0014" y2="12.5222" layer="94"/>
<rectangle x1="189.611" y1="12.3698" x2="191.7446" y2="12.5222" layer="94"/>
<rectangle x1="194.945" y1="12.3698" x2="196.9262" y2="12.5222" layer="94"/>
<rectangle x1="198.4502" y1="12.3698" x2="200.279" y2="12.5222" layer="94"/>
<rectangle x1="200.4314" y1="12.3698" x2="202.1078" y2="12.5222" layer="94"/>
<rectangle x1="202.7174" y1="12.3698" x2="204.3938" y2="12.5222" layer="94"/>
<rectangle x1="174.2186" y1="12.5222" x2="176.5046" y2="12.6746" layer="94"/>
<rectangle x1="179.5526" y1="12.5222" x2="181.3814" y2="12.6746" layer="94"/>
<rectangle x1="181.991" y1="12.5222" x2="183.9722" y2="12.6746" layer="94"/>
<rectangle x1="187.0202" y1="12.5222" x2="189.0014" y2="12.6746" layer="94"/>
<rectangle x1="189.611" y1="12.5222" x2="191.5922" y2="12.6746" layer="94"/>
<rectangle x1="194.945" y1="12.5222" x2="196.9262" y2="12.6746" layer="94"/>
<rectangle x1="198.4502" y1="12.5222" x2="200.279" y2="12.6746" layer="94"/>
<rectangle x1="200.4314" y1="12.5222" x2="202.1078" y2="12.6746" layer="94"/>
<rectangle x1="202.7174" y1="12.5222" x2="204.3938" y2="12.6746" layer="94"/>
<rectangle x1="174.2186" y1="12.6746" x2="176.3522" y2="12.827" layer="94"/>
<rectangle x1="179.4002" y1="12.6746" x2="181.3814" y2="12.827" layer="94"/>
<rectangle x1="181.991" y1="12.6746" x2="183.9722" y2="12.827" layer="94"/>
<rectangle x1="187.0202" y1="12.6746" x2="189.0014" y2="12.827" layer="94"/>
<rectangle x1="189.611" y1="12.6746" x2="191.5922" y2="12.827" layer="94"/>
<rectangle x1="194.945" y1="12.6746" x2="197.0786" y2="12.827" layer="94"/>
<rectangle x1="198.4502" y1="12.6746" x2="200.1266" y2="12.827" layer="94"/>
<rectangle x1="200.4314" y1="12.6746" x2="202.2602" y2="12.827" layer="94"/>
<rectangle x1="202.7174" y1="12.6746" x2="204.2414" y2="12.827" layer="94"/>
<rectangle x1="174.2186" y1="12.827" x2="176.5046" y2="12.9794" layer="94"/>
<rectangle x1="179.5526" y1="12.827" x2="181.3814" y2="12.9794" layer="94"/>
<rectangle x1="181.991" y1="12.827" x2="183.9722" y2="12.9794" layer="94"/>
<rectangle x1="187.0202" y1="12.827" x2="189.0014" y2="12.9794" layer="94"/>
<rectangle x1="189.611" y1="12.827" x2="191.5922" y2="12.9794" layer="94"/>
<rectangle x1="195.0974" y1="12.827" x2="197.0786" y2="12.9794" layer="94"/>
<rectangle x1="198.2978" y1="12.827" x2="200.1266" y2="12.9794" layer="94"/>
<rectangle x1="200.5838" y1="12.827" x2="202.2602" y2="12.9794" layer="94"/>
<rectangle x1="202.565" y1="12.827" x2="204.089" y2="12.9794" layer="94"/>
<rectangle x1="174.2186" y1="12.9794" x2="176.3522" y2="13.1318" layer="94"/>
<rectangle x1="179.4002" y1="12.9794" x2="181.3814" y2="13.1318" layer="94"/>
<rectangle x1="181.991" y1="12.9794" x2="183.9722" y2="13.1318" layer="94"/>
<rectangle x1="187.0202" y1="12.9794" x2="189.0014" y2="13.1318" layer="94"/>
<rectangle x1="189.4586" y1="12.9794" x2="191.4398" y2="13.1318" layer="94"/>
<rectangle x1="195.0974" y1="12.9794" x2="197.0786" y2="13.1318" layer="94"/>
<rectangle x1="198.2978" y1="12.9794" x2="200.1266" y2="13.1318" layer="94"/>
<rectangle x1="200.5838" y1="12.9794" x2="202.4126" y2="13.1318" layer="94"/>
<rectangle x1="202.565" y1="12.9794" x2="204.089" y2="13.1318" layer="94"/>
<rectangle x1="174.2186" y1="13.1318" x2="176.5046" y2="13.2842" layer="94"/>
<rectangle x1="179.5526" y1="13.1318" x2="181.3814" y2="13.2842" layer="94"/>
<rectangle x1="181.991" y1="13.1318" x2="183.9722" y2="13.2842" layer="94"/>
<rectangle x1="187.0202" y1="13.1318" x2="189.0014" y2="13.2842" layer="94"/>
<rectangle x1="189.4586" y1="13.1318" x2="191.4398" y2="13.2842" layer="94"/>
<rectangle x1="195.0974" y1="13.1318" x2="197.0786" y2="13.2842" layer="94"/>
<rectangle x1="198.1454" y1="13.1318" x2="199.9742" y2="13.2842" layer="94"/>
<rectangle x1="200.5838" y1="13.1318" x2="202.4126" y2="13.2842" layer="94"/>
<rectangle x1="202.565" y1="13.1318" x2="204.089" y2="13.2842" layer="94"/>
<rectangle x1="174.2186" y1="13.2842" x2="176.3522" y2="13.4366" layer="94"/>
<rectangle x1="179.4002" y1="13.2842" x2="181.3814" y2="13.4366" layer="94"/>
<rectangle x1="181.991" y1="13.2842" x2="183.9722" y2="13.4366" layer="94"/>
<rectangle x1="187.0202" y1="13.2842" x2="189.0014" y2="13.4366" layer="94"/>
<rectangle x1="189.4586" y1="13.2842" x2="191.4398" y2="13.4366" layer="94"/>
<rectangle x1="195.0974" y1="13.2842" x2="197.0786" y2="13.4366" layer="94"/>
<rectangle x1="198.1454" y1="13.2842" x2="199.9742" y2="13.4366" layer="94"/>
<rectangle x1="200.7362" y1="13.2842" x2="202.4126" y2="13.4366" layer="94"/>
<rectangle x1="202.565" y1="13.2842" x2="203.9366" y2="13.4366" layer="94"/>
<rectangle x1="174.2186" y1="13.4366" x2="176.5046" y2="13.589" layer="94"/>
<rectangle x1="179.5526" y1="13.4366" x2="181.3814" y2="13.589" layer="94"/>
<rectangle x1="181.991" y1="13.4366" x2="183.9722" y2="13.589" layer="94"/>
<rectangle x1="187.0202" y1="13.4366" x2="189.0014" y2="13.589" layer="94"/>
<rectangle x1="189.4586" y1="13.4366" x2="191.4398" y2="13.589" layer="94"/>
<rectangle x1="195.2498" y1="13.4366" x2="197.0786" y2="13.589" layer="94"/>
<rectangle x1="198.1454" y1="13.4366" x2="199.9742" y2="13.589" layer="94"/>
<rectangle x1="200.7362" y1="13.4366" x2="203.9366" y2="13.589" layer="94"/>
<rectangle x1="174.2186" y1="13.589" x2="176.3522" y2="13.7414" layer="94"/>
<rectangle x1="179.4002" y1="13.589" x2="181.3814" y2="13.7414" layer="94"/>
<rectangle x1="181.991" y1="13.589" x2="183.9722" y2="13.7414" layer="94"/>
<rectangle x1="187.0202" y1="13.589" x2="189.0014" y2="13.7414" layer="94"/>
<rectangle x1="189.4586" y1="13.589" x2="191.4398" y2="13.7414" layer="94"/>
<rectangle x1="195.2498" y1="13.589" x2="197.231" y2="13.7414" layer="94"/>
<rectangle x1="197.993" y1="13.589" x2="199.8218" y2="13.7414" layer="94"/>
<rectangle x1="200.8886" y1="13.589" x2="203.9366" y2="13.7414" layer="94"/>
<rectangle x1="174.2186" y1="13.7414" x2="176.5046" y2="13.8938" layer="94"/>
<rectangle x1="179.5526" y1="13.7414" x2="181.3814" y2="13.8938" layer="94"/>
<rectangle x1="181.991" y1="13.7414" x2="183.9722" y2="13.8938" layer="94"/>
<rectangle x1="187.0202" y1="13.7414" x2="189.0014" y2="13.8938" layer="94"/>
<rectangle x1="189.4586" y1="13.7414" x2="191.4398" y2="13.8938" layer="94"/>
<rectangle x1="195.2498" y1="13.7414" x2="197.0786" y2="13.8938" layer="94"/>
<rectangle x1="197.993" y1="13.7414" x2="199.8218" y2="13.8938" layer="94"/>
<rectangle x1="200.8886" y1="13.7414" x2="203.9366" y2="13.8938" layer="94"/>
<rectangle x1="174.2186" y1="13.8938" x2="176.3522" y2="14.0462" layer="94"/>
<rectangle x1="179.4002" y1="13.8938" x2="181.3814" y2="14.0462" layer="94"/>
<rectangle x1="181.991" y1="13.8938" x2="183.9722" y2="14.0462" layer="94"/>
<rectangle x1="187.0202" y1="13.8938" x2="189.0014" y2="14.0462" layer="94"/>
<rectangle x1="189.4586" y1="13.8938" x2="191.4398" y2="14.0462" layer="94"/>
<rectangle x1="195.2498" y1="13.8938" x2="197.231" y2="14.0462" layer="94"/>
<rectangle x1="197.8406" y1="13.8938" x2="199.8218" y2="14.0462" layer="94"/>
<rectangle x1="200.8886" y1="13.8938" x2="203.9366" y2="14.0462" layer="94"/>
<rectangle x1="174.2186" y1="14.0462" x2="176.5046" y2="14.1986" layer="94"/>
<rectangle x1="179.5526" y1="14.0462" x2="181.3814" y2="14.1986" layer="94"/>
<rectangle x1="181.991" y1="14.0462" x2="183.9722" y2="14.1986" layer="94"/>
<rectangle x1="187.0202" y1="14.0462" x2="189.0014" y2="14.1986" layer="94"/>
<rectangle x1="189.4586" y1="14.0462" x2="191.4398" y2="14.1986" layer="94"/>
<rectangle x1="195.2498" y1="14.0462" x2="197.231" y2="14.1986" layer="94"/>
<rectangle x1="197.8406" y1="14.0462" x2="199.6694" y2="14.1986" layer="94"/>
<rectangle x1="201.041" y1="14.0462" x2="203.9366" y2="14.1986" layer="94"/>
<rectangle x1="204.6986" y1="14.0462" x2="210.6422" y2="14.1986" layer="94"/>
<rectangle x1="174.2186" y1="14.1986" x2="176.3522" y2="14.351" layer="94"/>
<rectangle x1="179.4002" y1="14.1986" x2="181.3814" y2="14.351" layer="94"/>
<rectangle x1="181.991" y1="14.1986" x2="183.9722" y2="14.351" layer="94"/>
<rectangle x1="187.0202" y1="14.1986" x2="189.0014" y2="14.351" layer="94"/>
<rectangle x1="189.4586" y1="14.1986" x2="191.4398" y2="14.351" layer="94"/>
<rectangle x1="195.2498" y1="14.1986" x2="197.231" y2="14.351" layer="94"/>
<rectangle x1="197.8406" y1="14.1986" x2="199.6694" y2="14.351" layer="94"/>
<rectangle x1="201.041" y1="14.1986" x2="203.9366" y2="14.351" layer="94"/>
<rectangle x1="204.851" y1="14.1986" x2="210.6422" y2="14.351" layer="94"/>
<rectangle x1="174.2186" y1="14.351" x2="176.5046" y2="14.5034" layer="94"/>
<rectangle x1="179.5526" y1="14.351" x2="181.3814" y2="14.5034" layer="94"/>
<rectangle x1="181.991" y1="14.351" x2="183.9722" y2="14.5034" layer="94"/>
<rectangle x1="187.0202" y1="14.351" x2="189.0014" y2="14.5034" layer="94"/>
<rectangle x1="189.4586" y1="14.351" x2="191.4398" y2="14.5034" layer="94"/>
<rectangle x1="195.2498" y1="14.351" x2="197.0786" y2="14.5034" layer="94"/>
<rectangle x1="197.6882" y1="14.351" x2="199.517" y2="14.5034" layer="94"/>
<rectangle x1="201.041" y1="14.351" x2="203.9366" y2="14.5034" layer="94"/>
<rectangle x1="204.851" y1="14.351" x2="210.6422" y2="14.5034" layer="94"/>
<rectangle x1="174.2186" y1="14.5034" x2="176.5046" y2="14.6558" layer="94"/>
<rectangle x1="179.4002" y1="14.5034" x2="181.3814" y2="14.6558" layer="94"/>
<rectangle x1="181.991" y1="14.5034" x2="183.9722" y2="14.6558" layer="94"/>
<rectangle x1="187.0202" y1="14.5034" x2="189.0014" y2="14.6558" layer="94"/>
<rectangle x1="189.4586" y1="14.5034" x2="191.4398" y2="14.6558" layer="94"/>
<rectangle x1="195.2498" y1="14.5034" x2="197.231" y2="14.6558" layer="94"/>
<rectangle x1="197.6882" y1="14.5034" x2="199.517" y2="14.6558" layer="94"/>
<rectangle x1="201.1934" y1="14.5034" x2="203.9366" y2="14.6558" layer="94"/>
<rectangle x1="204.851" y1="14.5034" x2="210.6422" y2="14.6558" layer="94"/>
<rectangle x1="174.2186" y1="14.6558" x2="176.5046" y2="14.8082" layer="94"/>
<rectangle x1="179.5526" y1="14.6558" x2="181.3814" y2="14.8082" layer="94"/>
<rectangle x1="181.991" y1="14.6558" x2="183.9722" y2="14.8082" layer="94"/>
<rectangle x1="187.0202" y1="14.6558" x2="189.0014" y2="14.8082" layer="94"/>
<rectangle x1="189.4586" y1="14.6558" x2="191.4398" y2="14.8082" layer="94"/>
<rectangle x1="195.0974" y1="14.6558" x2="197.0786" y2="14.8082" layer="94"/>
<rectangle x1="197.5358" y1="14.6558" x2="199.517" y2="14.8082" layer="94"/>
<rectangle x1="201.1934" y1="14.6558" x2="203.9366" y2="14.8082" layer="94"/>
<rectangle x1="204.851" y1="14.6558" x2="210.6422" y2="14.8082" layer="94"/>
<rectangle x1="174.2186" y1="14.8082" x2="176.5046" y2="14.9606" layer="94"/>
<rectangle x1="179.4002" y1="14.8082" x2="181.3814" y2="14.9606" layer="94"/>
<rectangle x1="181.991" y1="14.8082" x2="184.1246" y2="14.9606" layer="94"/>
<rectangle x1="187.0202" y1="14.8082" x2="189.0014" y2="14.9606" layer="94"/>
<rectangle x1="189.4586" y1="14.8082" x2="191.4398" y2="14.9606" layer="94"/>
<rectangle x1="195.0974" y1="14.8082" x2="197.0786" y2="14.9606" layer="94"/>
<rectangle x1="197.5358" y1="14.8082" x2="199.3646" y2="14.9606" layer="94"/>
<rectangle x1="201.1934" y1="14.8082" x2="204.089" y2="14.9606" layer="94"/>
<rectangle x1="204.851" y1="14.8082" x2="210.4898" y2="14.9606" layer="94"/>
<rectangle x1="174.2186" y1="14.9606" x2="176.5046" y2="15.113" layer="94"/>
<rectangle x1="179.4002" y1="14.9606" x2="181.3814" y2="15.113" layer="94"/>
<rectangle x1="181.991" y1="14.9606" x2="183.9722" y2="15.113" layer="94"/>
<rectangle x1="187.0202" y1="14.9606" x2="189.0014" y2="15.113" layer="94"/>
<rectangle x1="189.4586" y1="14.9606" x2="191.4398" y2="15.113" layer="94"/>
<rectangle x1="195.0974" y1="14.9606" x2="197.0786" y2="15.113" layer="94"/>
<rectangle x1="197.5358" y1="14.9606" x2="199.3646" y2="15.113" layer="94"/>
<rectangle x1="201.3458" y1="14.9606" x2="204.089" y2="15.113" layer="94"/>
<rectangle x1="205.0034" y1="14.9606" x2="210.4898" y2="15.113" layer="94"/>
<rectangle x1="174.2186" y1="15.113" x2="176.5046" y2="15.2654" layer="94"/>
<rectangle x1="179.4002" y1="15.113" x2="181.3814" y2="15.2654" layer="94"/>
<rectangle x1="181.991" y1="15.113" x2="184.1246" y2="15.2654" layer="94"/>
<rectangle x1="187.0202" y1="15.113" x2="189.0014" y2="15.2654" layer="94"/>
<rectangle x1="189.611" y1="15.113" x2="191.5922" y2="15.2654" layer="94"/>
<rectangle x1="195.0974" y1="15.113" x2="197.0786" y2="15.2654" layer="94"/>
<rectangle x1="197.3834" y1="15.113" x2="199.3646" y2="15.2654" layer="94"/>
<rectangle x1="201.3458" y1="15.113" x2="204.2414" y2="15.2654" layer="94"/>
<rectangle x1="205.0034" y1="15.113" x2="210.4898" y2="15.2654" layer="94"/>
<rectangle x1="174.2186" y1="15.2654" x2="176.657" y2="15.4178" layer="94"/>
<rectangle x1="179.4002" y1="15.2654" x2="181.3814" y2="15.4178" layer="94"/>
<rectangle x1="181.991" y1="15.2654" x2="184.1246" y2="15.4178" layer="94"/>
<rectangle x1="187.0202" y1="15.2654" x2="189.0014" y2="15.4178" layer="94"/>
<rectangle x1="189.611" y1="15.2654" x2="191.5922" y2="15.4178" layer="94"/>
<rectangle x1="194.945" y1="15.2654" x2="196.9262" y2="15.4178" layer="94"/>
<rectangle x1="197.3834" y1="15.2654" x2="199.2122" y2="15.4178" layer="94"/>
<rectangle x1="201.4982" y1="15.2654" x2="204.2414" y2="15.4178" layer="94"/>
<rectangle x1="205.1558" y1="15.2654" x2="210.3374" y2="15.4178" layer="94"/>
<rectangle x1="174.2186" y1="15.4178" x2="176.657" y2="15.5702" layer="94"/>
<rectangle x1="179.4002" y1="15.4178" x2="181.3814" y2="15.5702" layer="94"/>
<rectangle x1="181.991" y1="15.4178" x2="184.277" y2="15.5702" layer="94"/>
<rectangle x1="186.8678" y1="15.4178" x2="189.0014" y2="15.5702" layer="94"/>
<rectangle x1="189.611" y1="15.4178" x2="191.7446" y2="15.5702" layer="94"/>
<rectangle x1="194.945" y1="15.4178" x2="196.9262" y2="15.5702" layer="94"/>
<rectangle x1="197.231" y1="15.4178" x2="199.2122" y2="15.5702" layer="94"/>
<rectangle x1="201.4982" y1="15.4178" x2="204.3938" y2="15.5702" layer="94"/>
<rectangle x1="205.3082" y1="15.4178" x2="210.3374" y2="15.5702" layer="94"/>
<rectangle x1="211.5566" y1="15.4178" x2="212.471" y2="15.5702" layer="94"/>
<rectangle x1="174.2186" y1="15.5702" x2="176.8094" y2="15.7226" layer="94"/>
<rectangle x1="179.4002" y1="15.5702" x2="181.3814" y2="15.7226" layer="94"/>
<rectangle x1="181.991" y1="15.5702" x2="184.277" y2="15.7226" layer="94"/>
<rectangle x1="186.8678" y1="15.5702" x2="189.0014" y2="15.7226" layer="94"/>
<rectangle x1="189.7634" y1="15.5702" x2="191.7446" y2="15.7226" layer="94"/>
<rectangle x1="194.7926" y1="15.5702" x2="196.9262" y2="15.7226" layer="94"/>
<rectangle x1="197.231" y1="15.5702" x2="199.2122" y2="15.7226" layer="94"/>
<rectangle x1="201.4982" y1="15.5702" x2="204.5462" y2="15.7226" layer="94"/>
<rectangle x1="208.661" y1="15.5702" x2="210.3374" y2="15.7226" layer="94"/>
<rectangle x1="211.4042" y1="15.5702" x2="213.5378" y2="15.7226" layer="94"/>
<rectangle x1="174.2186" y1="15.7226" x2="176.8094" y2="15.875" layer="94"/>
<rectangle x1="179.2478" y1="15.7226" x2="181.3814" y2="15.875" layer="94"/>
<rectangle x1="181.991" y1="15.7226" x2="184.4294" y2="15.875" layer="94"/>
<rectangle x1="186.8678" y1="15.7226" x2="188.849" y2="15.875" layer="94"/>
<rectangle x1="189.7634" y1="15.7226" x2="191.897" y2="15.875" layer="94"/>
<rectangle x1="194.7926" y1="15.7226" x2="196.7738" y2="15.875" layer="94"/>
<rectangle x1="197.231" y1="15.7226" x2="199.0598" y2="15.875" layer="94"/>
<rectangle x1="201.6506" y1="15.7226" x2="204.6986" y2="15.875" layer="94"/>
<rectangle x1="208.5086" y1="15.7226" x2="210.185" y2="15.875" layer="94"/>
<rectangle x1="211.4042" y1="15.7226" x2="213.6902" y2="15.875" layer="94"/>
<rectangle x1="174.2186" y1="15.875" x2="176.9618" y2="16.0274" layer="94"/>
<rectangle x1="179.2478" y1="15.875" x2="181.3814" y2="16.0274" layer="94"/>
<rectangle x1="181.991" y1="15.875" x2="184.5818" y2="16.0274" layer="94"/>
<rectangle x1="186.7154" y1="15.875" x2="189.0014" y2="16.0274" layer="94"/>
<rectangle x1="189.7634" y1="15.875" x2="192.0494" y2="16.0274" layer="94"/>
<rectangle x1="194.6402" y1="15.875" x2="196.7738" y2="16.0274" layer="94"/>
<rectangle x1="197.0786" y1="15.875" x2="199.0598" y2="16.0274" layer="94"/>
<rectangle x1="201.6506" y1="15.875" x2="204.6986" y2="16.0274" layer="94"/>
<rectangle x1="208.3562" y1="15.875" x2="210.185" y2="16.0274" layer="94"/>
<rectangle x1="211.5566" y1="15.875" x2="213.6902" y2="16.0274" layer="94"/>
<rectangle x1="174.2186" y1="16.0274" x2="177.1142" y2="16.1798" layer="94"/>
<rectangle x1="179.0954" y1="16.0274" x2="181.3814" y2="16.1798" layer="94"/>
<rectangle x1="181.991" y1="16.0274" x2="184.7342" y2="16.1798" layer="94"/>
<rectangle x1="186.7154" y1="16.0274" x2="188.849" y2="16.1798" layer="94"/>
<rectangle x1="189.9158" y1="16.0274" x2="192.0494" y2="16.1798" layer="94"/>
<rectangle x1="194.4878" y1="16.0274" x2="196.6214" y2="16.1798" layer="94"/>
<rectangle x1="197.0786" y1="16.0274" x2="199.0598" y2="16.1798" layer="94"/>
<rectangle x1="201.6506" y1="16.0274" x2="205.0034" y2="16.1798" layer="94"/>
<rectangle x1="208.2038" y1="16.0274" x2="210.0326" y2="16.1798" layer="94"/>
<rectangle x1="212.0138" y1="16.0274" x2="213.5378" y2="16.1798" layer="94"/>
<rectangle x1="174.2186" y1="16.1798" x2="177.419" y2="16.3322" layer="94"/>
<rectangle x1="178.943" y1="16.1798" x2="181.229" y2="16.3322" layer="94"/>
<rectangle x1="181.991" y1="16.1798" x2="184.8866" y2="16.3322" layer="94"/>
<rectangle x1="186.563" y1="16.1798" x2="188.849" y2="16.3322" layer="94"/>
<rectangle x1="189.9158" y1="16.1798" x2="192.3542" y2="16.3322" layer="94"/>
<rectangle x1="194.3354" y1="16.1798" x2="196.6214" y2="16.3322" layer="94"/>
<rectangle x1="196.9262" y1="16.1798" x2="198.9074" y2="16.3322" layer="94"/>
<rectangle x1="201.803" y1="16.1798" x2="205.1558" y2="16.3322" layer="94"/>
<rectangle x1="207.899" y1="16.1798" x2="209.8802" y2="16.3322" layer="94"/>
<rectangle x1="212.471" y1="16.1798" x2="213.5378" y2="16.3322" layer="94"/>
<rectangle x1="174.2186" y1="16.3322" x2="177.7238" y2="16.4846" layer="94"/>
<rectangle x1="178.4858" y1="16.3322" x2="181.229" y2="16.4846" layer="94"/>
<rectangle x1="181.991" y1="16.3322" x2="185.3438" y2="16.4846" layer="94"/>
<rectangle x1="186.1058" y1="16.3322" x2="188.849" y2="16.4846" layer="94"/>
<rectangle x1="190.0682" y1="16.3322" x2="192.659" y2="16.4846" layer="94"/>
<rectangle x1="193.8782" y1="16.3322" x2="196.469" y2="16.4846" layer="94"/>
<rectangle x1="196.9262" y1="16.3322" x2="198.9074" y2="16.4846" layer="94"/>
<rectangle x1="201.803" y1="16.3322" x2="205.4606" y2="16.4846" layer="94"/>
<rectangle x1="207.5942" y1="16.3322" x2="209.8802" y2="16.4846" layer="94"/>
<rectangle x1="212.9282" y1="16.3322" x2="213.5378" y2="16.4846" layer="94"/>
<rectangle x1="174.2186" y1="16.4846" x2="181.229" y2="16.637" layer="94"/>
<rectangle x1="181.991" y1="16.4846" x2="188.6966" y2="16.637" layer="94"/>
<rectangle x1="190.2206" y1="16.4846" x2="196.3166" y2="16.637" layer="94"/>
<rectangle x1="196.9262" y1="16.4846" x2="198.755" y2="16.637" layer="94"/>
<rectangle x1="201.803" y1="16.4846" x2="205.9178" y2="16.637" layer="94"/>
<rectangle x1="207.137" y1="16.4846" x2="209.7278" y2="16.637" layer="94"/>
<rectangle x1="174.2186" y1="16.637" x2="181.0766" y2="16.7894" layer="94"/>
<rectangle x1="181.991" y1="16.637" x2="188.6966" y2="16.7894" layer="94"/>
<rectangle x1="190.373" y1="16.637" x2="196.3166" y2="16.7894" layer="94"/>
<rectangle x1="196.7738" y1="16.637" x2="198.755" y2="16.7894" layer="94"/>
<rectangle x1="201.9554" y1="16.637" x2="209.5754" y2="16.7894" layer="94"/>
<rectangle x1="174.2186" y1="16.7894" x2="176.3522" y2="16.9418" layer="94"/>
<rectangle x1="176.5046" y1="16.7894" x2="181.0766" y2="16.9418" layer="94"/>
<rectangle x1="181.991" y1="16.7894" x2="183.8198" y2="16.9418" layer="94"/>
<rectangle x1="183.9722" y1="16.7894" x2="188.5442" y2="16.9418" layer="94"/>
<rectangle x1="190.5254" y1="16.7894" x2="196.1642" y2="16.9418" layer="94"/>
<rectangle x1="196.7738" y1="16.7894" x2="198.755" y2="16.9418" layer="94"/>
<rectangle x1="201.9554" y1="16.7894" x2="209.423" y2="16.9418" layer="94"/>
<rectangle x1="174.2186" y1="16.9418" x2="176.3522" y2="17.0942" layer="94"/>
<rectangle x1="176.657" y1="16.9418" x2="180.9242" y2="17.0942" layer="94"/>
<rectangle x1="181.991" y1="16.9418" x2="183.8198" y2="17.0942" layer="94"/>
<rectangle x1="184.1246" y1="16.9418" x2="188.3918" y2="17.0942" layer="94"/>
<rectangle x1="190.5254" y1="16.9418" x2="196.0118" y2="17.0942" layer="94"/>
<rectangle x1="196.7738" y1="16.9418" x2="198.6026" y2="17.0942" layer="94"/>
<rectangle x1="202.1078" y1="16.9418" x2="209.2706" y2="17.0942" layer="94"/>
<rectangle x1="174.2186" y1="17.0942" x2="176.3522" y2="17.2466" layer="94"/>
<rectangle x1="176.8094" y1="17.0942" x2="180.7718" y2="17.2466" layer="94"/>
<rectangle x1="181.991" y1="17.0942" x2="183.8198" y2="17.2466" layer="94"/>
<rectangle x1="184.277" y1="17.0942" x2="188.3918" y2="17.2466" layer="94"/>
<rectangle x1="190.8302" y1="17.0942" x2="195.8594" y2="17.2466" layer="94"/>
<rectangle x1="196.7738" y1="17.0942" x2="198.6026" y2="17.2466" layer="94"/>
<rectangle x1="202.1078" y1="17.0942" x2="209.1182" y2="17.2466" layer="94"/>
<rectangle x1="174.2186" y1="17.2466" x2="176.3522" y2="17.399" layer="94"/>
<rectangle x1="176.9618" y1="17.2466" x2="180.6194" y2="17.399" layer="94"/>
<rectangle x1="182.1434" y1="17.2466" x2="183.8198" y2="17.399" layer="94"/>
<rectangle x1="184.4294" y1="17.2466" x2="188.2394" y2="17.399" layer="94"/>
<rectangle x1="190.9826" y1="17.2466" x2="195.5546" y2="17.399" layer="94"/>
<rectangle x1="196.7738" y1="17.2466" x2="198.6026" y2="17.399" layer="94"/>
<rectangle x1="202.1078" y1="17.2466" x2="203.9366" y2="17.399" layer="94"/>
<rectangle x1="204.2414" y1="17.2466" x2="208.9658" y2="17.399" layer="94"/>
<rectangle x1="174.2186" y1="17.399" x2="176.3522" y2="17.5514" layer="94"/>
<rectangle x1="177.1142" y1="17.399" x2="180.467" y2="17.5514" layer="94"/>
<rectangle x1="182.1434" y1="17.399" x2="183.6674" y2="17.5514" layer="94"/>
<rectangle x1="184.7342" y1="17.399" x2="187.9346" y2="17.5514" layer="94"/>
<rectangle x1="191.2874" y1="17.399" x2="195.2498" y2="17.5514" layer="94"/>
<rectangle x1="196.9262" y1="17.399" x2="198.4502" y2="17.5514" layer="94"/>
<rectangle x1="202.2602" y1="17.399" x2="203.7842" y2="17.5514" layer="94"/>
<rectangle x1="204.3938" y1="17.399" x2="208.661" y2="17.5514" layer="94"/>
<rectangle x1="174.2186" y1="17.5514" x2="176.1998" y2="17.7038" layer="94"/>
<rectangle x1="177.419" y1="17.5514" x2="180.1622" y2="17.7038" layer="94"/>
<rectangle x1="182.2958" y1="17.5514" x2="183.6674" y2="17.7038" layer="94"/>
<rectangle x1="184.8866" y1="17.5514" x2="187.7822" y2="17.7038" layer="94"/>
<rectangle x1="191.5922" y1="17.5514" x2="195.0974" y2="17.7038" layer="94"/>
<rectangle x1="196.9262" y1="17.5514" x2="198.2978" y2="17.7038" layer="94"/>
<rectangle x1="202.4126" y1="17.5514" x2="203.7842" y2="17.7038" layer="94"/>
<rectangle x1="204.6986" y1="17.5514" x2="208.3562" y2="17.7038" layer="94"/>
<rectangle x1="210.4898" y1="17.5514" x2="210.6422" y2="17.7038" layer="94"/>
<rectangle x1="174.2186" y1="17.7038" x2="176.3522" y2="17.8562" layer="94"/>
<rectangle x1="177.7238" y1="17.7038" x2="179.8574" y2="17.8562" layer="94"/>
<rectangle x1="182.2958" y1="17.7038" x2="183.515" y2="17.8562" layer="94"/>
<rectangle x1="185.1914" y1="17.7038" x2="187.4774" y2="17.8562" layer="94"/>
<rectangle x1="191.897" y1="17.7038" x2="194.6402" y2="17.8562" layer="94"/>
<rectangle x1="197.0786" y1="17.7038" x2="198.2978" y2="17.8562" layer="94"/>
<rectangle x1="202.4126" y1="17.7038" x2="203.4794" y2="17.8562" layer="94"/>
<rectangle x1="205.0034" y1="17.7038" x2="208.0514" y2="17.8562" layer="94"/>
<rectangle x1="210.3374" y1="17.7038" x2="210.7946" y2="17.8562" layer="94"/>
<rectangle x1="174.371" y1="17.8562" x2="176.1998" y2="18.0086" layer="94"/>
<rectangle x1="178.181" y1="17.8562" x2="179.2478" y2="18.0086" layer="94"/>
<rectangle x1="182.6006" y1="17.8562" x2="183.2102" y2="18.0086" layer="94"/>
<rectangle x1="185.801" y1="17.8562" x2="186.7154" y2="18.0086" layer="94"/>
<rectangle x1="186.8678" y1="17.8562" x2="187.0202" y2="18.0086" layer="94"/>
<rectangle x1="192.5066" y1="17.8562" x2="192.659" y2="18.0086" layer="94"/>
<rectangle x1="192.8114" y1="17.8562" x2="193.7258" y2="18.0086" layer="94"/>
<rectangle x1="193.8782" y1="17.8562" x2="194.0306" y2="18.0086" layer="94"/>
<rectangle x1="197.3834" y1="17.8562" x2="197.993" y2="18.0086" layer="94"/>
<rectangle x1="202.7174" y1="17.8562" x2="203.1746" y2="18.0086" layer="94"/>
<rectangle x1="205.4606" y1="17.8562" x2="207.5942" y2="18.0086" layer="94"/>
<rectangle x1="210.185" y1="17.8562" x2="211.0994" y2="18.0086" layer="94"/>
<rectangle x1="174.371" y1="18.0086" x2="176.1998" y2="18.161" layer="94"/>
<rectangle x1="206.2226" y1="18.0086" x2="206.375" y2="18.161" layer="94"/>
<rectangle x1="206.5274" y1="18.0086" x2="206.6798" y2="18.161" layer="94"/>
<rectangle x1="210.3374" y1="18.0086" x2="211.2518" y2="18.161" layer="94"/>
<rectangle x1="174.5234" y1="18.161" x2="176.0474" y2="18.3134" layer="94"/>
<rectangle x1="210.4898" y1="18.161" x2="211.5566" y2="18.3134" layer="94"/>
<rectangle x1="174.6758" y1="18.3134" x2="175.895" y2="18.4658" layer="94"/>
<rectangle x1="210.6422" y1="18.3134" x2="211.709" y2="18.4658" layer="94"/>
<rectangle x1="174.8282" y1="18.4658" x2="175.5902" y2="18.6182" layer="94"/>
<rectangle x1="210.7946" y1="18.4658" x2="212.0138" y2="18.6182" layer="94"/>
<rectangle x1="203.327" y1="18.6182" x2="203.7842" y2="18.7706" layer="94"/>
<rectangle x1="210.947" y1="18.6182" x2="212.1662" y2="18.7706" layer="94"/>
<rectangle x1="203.327" y1="18.7706" x2="203.9366" y2="18.923" layer="94"/>
<rectangle x1="211.0994" y1="18.7706" x2="212.3186" y2="18.923" layer="94"/>
<rectangle x1="203.1746" y1="18.923" x2="203.7842" y2="19.0754" layer="94"/>
<rectangle x1="211.2518" y1="18.923" x2="212.1662" y2="19.0754" layer="94"/>
<rectangle x1="174.6758" y1="19.0754" x2="175.895" y2="19.2278" layer="94"/>
<rectangle x1="203.0222" y1="19.0754" x2="203.7842" y2="19.2278" layer="94"/>
<rectangle x1="208.3562" y1="19.0754" x2="208.661" y2="19.2278" layer="94"/>
<rectangle x1="211.4042" y1="19.0754" x2="212.0138" y2="19.2278" layer="94"/>
<rectangle x1="174.5234" y1="19.2278" x2="176.0474" y2="19.3802" layer="94"/>
<rectangle x1="202.8698" y1="19.2278" x2="203.6318" y2="19.3802" layer="94"/>
<rectangle x1="208.2038" y1="19.2278" x2="208.8134" y2="19.3802" layer="94"/>
<rectangle x1="211.4042" y1="19.2278" x2="211.8614" y2="19.3802" layer="94"/>
<rectangle x1="174.2186" y1="19.3802" x2="176.1998" y2="19.5326" layer="94"/>
<rectangle x1="202.8698" y1="19.3802" x2="203.6318" y2="19.5326" layer="94"/>
<rectangle x1="205.7654" y1="19.3802" x2="205.9178" y2="19.5326" layer="94"/>
<rectangle x1="206.0702" y1="19.3802" x2="206.2226" y2="19.5326" layer="94"/>
<rectangle x1="208.2038" y1="19.3802" x2="208.8134" y2="19.5326" layer="94"/>
<rectangle x1="174.2186" y1="19.5326" x2="176.3522" y2="19.685" layer="94"/>
<rectangle x1="202.7174" y1="19.5326" x2="203.4794" y2="19.685" layer="94"/>
<rectangle x1="205.7654" y1="19.5326" x2="206.2226" y2="19.685" layer="94"/>
<rectangle x1="208.2038" y1="19.5326" x2="208.9658" y2="19.685" layer="94"/>
<rectangle x1="174.2186" y1="19.685" x2="176.3522" y2="19.8374" layer="94"/>
<rectangle x1="202.565" y1="19.685" x2="203.4794" y2="19.8374" layer="94"/>
<rectangle x1="205.613" y1="19.685" x2="206.2226" y2="19.8374" layer="94"/>
<rectangle x1="208.3562" y1="19.685" x2="208.9658" y2="19.8374" layer="94"/>
<rectangle x1="174.0662" y1="19.8374" x2="176.3522" y2="19.9898" layer="94"/>
<rectangle x1="202.4126" y1="19.8374" x2="203.4794" y2="19.9898" layer="94"/>
<rectangle x1="205.613" y1="19.8374" x2="206.2226" y2="19.9898" layer="94"/>
<rectangle x1="208.3562" y1="19.8374" x2="209.1182" y2="19.9898" layer="94"/>
<rectangle x1="174.0662" y1="19.9898" x2="176.5046" y2="20.1422" layer="94"/>
<rectangle x1="202.2602" y1="19.9898" x2="203.327" y2="20.1422" layer="94"/>
<rectangle x1="205.613" y1="19.9898" x2="206.2226" y2="20.1422" layer="94"/>
<rectangle x1="208.3562" y1="19.9898" x2="209.1182" y2="20.1422" layer="94"/>
<rectangle x1="174.0662" y1="20.1422" x2="176.5046" y2="20.2946" layer="94"/>
<rectangle x1="202.2602" y1="20.1422" x2="203.327" y2="20.2946" layer="94"/>
<rectangle x1="205.613" y1="20.1422" x2="206.2226" y2="20.2946" layer="94"/>
<rectangle x1="208.3562" y1="20.1422" x2="209.2706" y2="20.2946" layer="94"/>
<rectangle x1="174.0662" y1="20.2946" x2="176.5046" y2="20.447" layer="94"/>
<rectangle x1="202.4126" y1="20.2946" x2="203.1746" y2="20.447" layer="94"/>
<rectangle x1="205.613" y1="20.2946" x2="206.2226" y2="20.447" layer="94"/>
<rectangle x1="208.5086" y1="20.2946" x2="209.2706" y2="20.447" layer="94"/>
<rectangle x1="174.0662" y1="20.447" x2="176.3522" y2="20.5994" layer="94"/>
<rectangle x1="202.565" y1="20.447" x2="203.1746" y2="20.5994" layer="94"/>
<rectangle x1="205.613" y1="20.447" x2="206.2226" y2="20.5994" layer="94"/>
<rectangle x1="208.5086" y1="20.447" x2="209.423" y2="20.5994" layer="94"/>
<rectangle x1="174.2186" y1="20.5994" x2="176.3522" y2="20.7518" layer="94"/>
<rectangle x1="202.8698" y1="20.5994" x2="203.0222" y2="20.7518" layer="94"/>
<rectangle x1="205.4606" y1="20.5994" x2="206.2226" y2="20.7518" layer="94"/>
<rectangle x1="208.5086" y1="20.5994" x2="209.5754" y2="20.7518" layer="94"/>
<rectangle x1="174.2186" y1="20.7518" x2="176.3522" y2="20.9042" layer="94"/>
<rectangle x1="205.4606" y1="20.7518" x2="206.2226" y2="20.9042" layer="94"/>
<rectangle x1="208.661" y1="20.7518" x2="209.5754" y2="20.9042" layer="94"/>
<rectangle x1="174.371" y1="20.9042" x2="176.1998" y2="21.0566" layer="94"/>
<rectangle x1="205.4606" y1="20.9042" x2="206.2226" y2="21.0566" layer="94"/>
<rectangle x1="208.661" y1="20.9042" x2="209.5754" y2="21.0566" layer="94"/>
<rectangle x1="174.5234" y1="21.0566" x2="176.0474" y2="21.209" layer="94"/>
<rectangle x1="205.4606" y1="21.0566" x2="206.375" y2="21.209" layer="94"/>
<rectangle x1="208.661" y1="21.0566" x2="209.2706" y2="21.209" layer="94"/>
<rectangle x1="174.8282" y1="21.209" x2="175.5902" y2="21.3614" layer="94"/>
<rectangle x1="205.4606" y1="21.209" x2="206.2226" y2="21.3614" layer="94"/>
<rectangle x1="205.3082" y1="21.3614" x2="206.375" y2="21.5138" layer="94"/>
<rectangle x1="205.4606" y1="21.5138" x2="206.2226" y2="21.6662" layer="94"/>
</symbol>
</symbols>
<devicesets>
<deviceset name="A4L" prefix="FRAME" uservalue="yes">
<description>&lt;b&gt;A4 Landscape Frame&lt;/b&gt;
&lt;p&gt;With company logo&lt;/p&gt;</description>
<gates>
<gate name="G$1" symbol="A4L" x="0" y="0"/>
</gates>
<devices>
<device name="">
<technologies>
<technology name=""/>
</technologies>
</device>
</devices>
</deviceset>
</devicesets>
</library>
<library name="innove-bjts">
<description>&lt;b&gt;BJTs&lt;/b&gt; - &lt;i&gt;Bipolar Junction Transistors&lt;/i&gt;

&lt;p&gt;A bipolar junction transistor (BJT) is a type of transistor that uses both electrons and holes as charge carriers. Unipolar transistors, such as field-effect transistors, use only one kind of charge carrier. A bipolar transistor allows a small current injected at one of its terminals to control a much larger current flowing between two other terminals, making the device capable of amplification or switching.&lt;/p&gt;

&lt;p&gt;BJTs use two junctions between two semiconductor types, n-type and p-type, which are regions in a single crystal of material. The junctions can be made in several different ways, such as changing the doping of the semiconductor material as it is grown, by depositing metal pellets to form alloy junctions, or by such methods as diffusion of n -type and p-type doping substances into the crystal. The superior predictability and performance of junction transistors soon displaced the original point-contact transistor. Diffused transistors, along with other components, are elements of integrated circuits for analog and digital functions. Hundreds of bipolar junction transistors can be made in one circuit at very low cost.&lt;/p&gt;

&lt;p&gt;&lt;author&gt;Created by Nathan Campos &amp;lt;nathan@innoveworkshop.com&amp;gt;&lt;/author&gt;&lt;/p&gt;</description>
<packages>
<package name="TO92">
<description>&lt;b&gt;TO 92&lt;/b&gt;</description>
<wire x1="-2.0946" y1="-1.651" x2="-2.6549" y2="-0.254" width="0.127" layer="21" curve="-32.781"/>
<wire x1="-2.6549" y1="-0.254" x2="-0.7863" y2="2.5485" width="0.127" layer="21" curve="-78.3185"/>
<wire x1="0.7863" y1="2.5484" x2="2.0945" y2="-1.651" width="0.127" layer="21" curve="-111.1"/>
<wire x1="-2.0945" y1="-1.651" x2="2.0945" y2="-1.651" width="0.127" layer="21"/>
<wire x1="-2.2537" y1="-0.254" x2="-0.2863" y2="-0.254" width="0.127" layer="51"/>
<wire x1="-2.6549" y1="-0.254" x2="-2.2537" y2="-0.254" width="0.127" layer="21"/>
<wire x1="-0.2863" y1="-0.254" x2="0.2863" y2="-0.254" width="0.127" layer="21"/>
<wire x1="2.2537" y1="-0.254" x2="2.6549" y2="-0.254" width="0.127" layer="21"/>
<wire x1="0.2863" y1="-0.254" x2="2.2537" y2="-0.254" width="0.127" layer="51"/>
<wire x1="-0.7863" y1="2.5485" x2="0.7863" y2="2.5485" width="0.127" layer="51" curve="-34.2936"/>
<pad name="3" x="1.27" y="0" drill="0.8128" shape="octagon"/>
<pad name="2" x="0" y="1.905" drill="0.8128" shape="octagon"/>
<pad name="1" x="-1.27" y="0" drill="0.8128" shape="octagon"/>
<text x="0" y="-2.54" size="1.27" layer="25" ratio="10" align="center">&gt;NAME</text>
<text x="0" y="3.175" size="1.27" layer="27" ratio="10" align="bottom-center">&gt;VALUE</text>
</package>
<package name="TO126V">
<description>&lt;b&gt;TO 126 vertical&lt;/b&gt;</description>
<wire x1="-3.937" y1="-0.127" x2="-3.937" y2="-1.27" width="0.127" layer="21"/>
<wire x1="-3.937" y1="-1.27" x2="-3.683" y2="-2.794" width="0.127" layer="21"/>
<wire x1="3.683" y1="-2.794" x2="3.937" y2="-1.27" width="0.127" layer="21"/>
<wire x1="3.937" y1="-1.27" x2="3.937" y2="-0.127" width="0.127" layer="21"/>
<wire x1="-3.683" y1="-2.794" x2="-2.794" y2="-2.794" width="0.127" layer="21"/>
<wire x1="-2.794" y1="-2.794" x2="-1.778" y2="-2.794" width="0.127" layer="51"/>
<wire x1="-1.778" y1="-2.794" x2="-0.508" y2="-2.794" width="0.127" layer="21"/>
<wire x1="-0.508" y1="-2.794" x2="0.508" y2="-2.794" width="0.127" layer="51"/>
<wire x1="0.508" y1="-2.794" x2="1.778" y2="-2.794" width="0.127" layer="21"/>
<wire x1="1.778" y1="-2.794" x2="2.794" y2="-2.794" width="0.127" layer="51"/>
<wire x1="2.794" y1="-2.794" x2="3.683" y2="-2.794" width="0.127" layer="21"/>
<circle x="-3.175" y="-2.159" radius="0.4064" width="0.127" layer="51"/>
<pad name="1" x="-2.286" y="-1.27" drill="1.016" shape="long" rot="R90"/>
<pad name="2" x="0" y="-1.27" drill="1.016" shape="long" rot="R90"/>
<pad name="3" x="2.286" y="-1.27" drill="1.016" shape="long" rot="R90"/>
<text x="-3.9624" y="-4.5466" size="1.27" layer="25" ratio="10">&gt;NAME</text>
<text x="-3.9878" y="-6.3246" size="1.27" layer="27" ratio="10">&gt;VALUE</text>
<text x="-2.54" y="-1.905" size="1.27" layer="51" ratio="10">1</text>
<text x="-0.381" y="-1.905" size="1.27" layer="51" ratio="10">2</text>
<text x="2.032" y="-1.905" size="1.27" layer="51" ratio="10">3</text>
<rectangle x1="-3.937" y1="-0.381" x2="-3.175" y2="0" layer="21"/>
<rectangle x1="-1.397" y1="-0.381" x2="-0.889" y2="0" layer="21"/>
<rectangle x1="0.889" y1="-0.381" x2="1.397" y2="0" layer="21"/>
<rectangle x1="3.175" y1="-0.381" x2="3.937" y2="0" layer="21"/>
<rectangle x1="-3.175" y1="-0.381" x2="-1.397" y2="0" layer="51"/>
<rectangle x1="-0.889" y1="-0.381" x2="0.889" y2="0" layer="51"/>
<rectangle x1="1.397" y1="-0.381" x2="3.175" y2="0" layer="51"/>
</package>
</packages>
<symbols>
<symbol name="NPN">
<wire x1="2.54" y1="2.54" x2="0.508" y2="1.524" width="0.1524" layer="94"/>
<wire x1="1.778" y1="-1.524" x2="2.54" y2="-2.54" width="0.1524" layer="94"/>
<wire x1="2.54" y1="-2.54" x2="1.27" y2="-2.54" width="0.1524" layer="94"/>
<wire x1="1.27" y1="-2.54" x2="1.778" y2="-1.524" width="0.1524" layer="94"/>
<wire x1="1.54" y1="-2.04" x2="0.308" y2="-1.424" width="0.1524" layer="94"/>
<wire x1="1.524" y1="-2.413" x2="2.286" y2="-2.413" width="0.254" layer="94"/>
<wire x1="2.286" y1="-2.413" x2="1.778" y2="-1.778" width="0.254" layer="94"/>
<wire x1="1.778" y1="-1.778" x2="1.524" y2="-2.286" width="0.254" layer="94"/>
<wire x1="1.524" y1="-2.286" x2="1.905" y2="-2.286" width="0.254" layer="94"/>
<wire x1="1.905" y1="-2.286" x2="1.778" y2="-2.032" width="0.254" layer="94"/>
<text x="4.318" y="0.508" size="1.778" layer="95">&gt;NAME</text>
<text x="4.318" y="-2.032" size="1.778" layer="96">&gt;VALUE</text>
<rectangle x1="-0.254" y1="-2.54" x2="0.508" y2="2.54" layer="94"/>
<pin name="B" x="-2.54" y="0" visible="off" length="short" direction="pas" swaplevel="1"/>
<pin name="E" x="2.54" y="-5.08" visible="off" length="short" direction="pas" swaplevel="3" rot="R90"/>
<pin name="C" x="2.54" y="5.08" visible="off" length="short" direction="pas" swaplevel="2" rot="R270"/>
</symbol>
<symbol name="PNP">
<wire x1="2.0861" y1="1.6779" x2="1.5781" y2="2.5941" width="0.1524" layer="94"/>
<wire x1="1.5781" y1="2.5941" x2="0.5159" y2="1.478" width="0.1524" layer="94"/>
<wire x1="0.5159" y1="1.478" x2="2.0861" y2="1.6779" width="0.1524" layer="94"/>
<wire x1="2.54" y1="2.54" x2="1.808" y2="2.1239" width="0.1524" layer="94"/>
<wire x1="2.54" y1="-2.54" x2="0.508" y2="-1.524" width="0.1524" layer="94"/>
<wire x1="1.905" y1="1.778" x2="1.524" y2="2.413" width="0.254" layer="94"/>
<wire x1="1.524" y1="2.413" x2="0.762" y2="1.651" width="0.254" layer="94"/>
<wire x1="0.762" y1="1.651" x2="1.778" y2="1.778" width="0.254" layer="94"/>
<wire x1="1.778" y1="1.778" x2="1.524" y2="2.159" width="0.254" layer="94"/>
<wire x1="1.524" y1="2.159" x2="1.143" y2="1.905" width="0.254" layer="94"/>
<wire x1="1.143" y1="1.905" x2="1.524" y2="1.905" width="0.254" layer="94"/>
<text x="4.064" y="0.508" size="1.778" layer="95">&gt;NAME</text>
<text x="4.064" y="-2.032" size="1.778" layer="96">&gt;VALUE</text>
<rectangle x1="-0.254" y1="-2.54" x2="0.508" y2="2.54" layer="94"/>
<pin name="B" x="-2.54" y="0" visible="off" length="short" direction="pas"/>
<pin name="E" x="2.54" y="5.08" visible="off" length="short" direction="pas" rot="R270"/>
<pin name="C" x="2.54" y="-5.08" visible="off" length="short" direction="pas" rot="R90"/>
</symbol>
</symbols>
<devicesets>
<deviceset name="BC547" prefix="Q">
<gates>
<gate name="G$1" symbol="NPN" x="0" y="0"/>
</gates>
<devices>
<device name="" package="TO92">
<connects>
<connect gate="G$1" pin="B" pad="2"/>
<connect gate="G$1" pin="C" pad="1"/>
<connect gate="G$1" pin="E" pad="3"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
</devices>
</deviceset>
<deviceset name="BC559" prefix="Q">
<gates>
<gate name="G$1" symbol="PNP" x="0" y="0"/>
</gates>
<devices>
<device name="" package="TO92">
<connects>
<connect gate="G$1" pin="B" pad="2"/>
<connect gate="G$1" pin="C" pad="1"/>
<connect gate="G$1" pin="E" pad="3"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
</devices>
</deviceset>
<deviceset name="BD140" prefix="Q">
<gates>
<gate name="G$1" symbol="PNP" x="0" y="0"/>
</gates>
<devices>
<device name="" package="TO126V">
<connects>
<connect gate="G$1" pin="B" pad="3"/>
<connect gate="G$1" pin="C" pad="2"/>
<connect gate="G$1" pin="E" pad="1"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
</devices>
</deviceset>
<deviceset name="BD139" prefix="Q">
<gates>
<gate name="G$1" symbol="NPN" x="0" y="0"/>
</gates>
<devices>
<device name="" package="TO126V">
<connects>
<connect gate="G$1" pin="B" pad="3"/>
<connect gate="G$1" pin="C" pad="2"/>
<connect gate="G$1" pin="E" pad="1"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
</devices>
</deviceset>
</devicesets>
</library>
<library name="innove-passives">
<description>&lt;b&gt;Passives&lt;/b&gt;

&lt;i&gt;Resistors, Capacitors, and Inductors&lt;/i&gt;

&lt;p&gt;Our collection of the most boring components ever created.&lt;/p&gt;

&lt;p&gt;By the way, we use the IEC style of schematic symbols, which everyone knows is better, so deal with it.&lt;/p&gt;

&lt;p&gt;&lt;author&gt;Created by Nathan Campos &amp;lt;nathan@innoveworkshop.com&amp;gt;&lt;/author&gt;&lt;/p&gt;</description>
<packages>
<package name="R0805">
<description>&lt;b&gt;RESISTOR&lt;/b&gt;&lt;p&gt;</description>
<wire x1="-0.41" y1="0.635" x2="0.41" y2="0.635" width="0.1524" layer="51"/>
<wire x1="-0.41" y1="-0.635" x2="0.41" y2="-0.635" width="0.1524" layer="51"/>
<wire x1="-1.973" y1="0.983" x2="1.973" y2="0.983" width="0.0508" layer="39"/>
<wire x1="1.973" y1="0.983" x2="1.973" y2="-0.983" width="0.0508" layer="39"/>
<wire x1="1.973" y1="-0.983" x2="-1.973" y2="-0.983" width="0.0508" layer="39"/>
<wire x1="-1.973" y1="-0.983" x2="-1.973" y2="0.983" width="0.0508" layer="39"/>
<smd name="1" x="-0.95" y="0" dx="1.3" dy="1.5" layer="1"/>
<smd name="2" x="0.95" y="0" dx="1.3" dy="1.5" layer="1"/>
<text x="-0.635" y="1.27" size="1.27" layer="25">&gt;NAME</text>
<text x="-0.635" y="-2.54" size="1.27" layer="27">&gt;VALUE</text>
<rectangle x1="0.4064" y1="-0.6985" x2="1.0564" y2="0.7015" layer="51"/>
<rectangle x1="-1.0668" y1="-0.6985" x2="-0.4168" y2="0.7015" layer="51"/>
<rectangle x1="-0.1999" y1="-0.5001" x2="0.1999" y2="0.5001" layer="35"/>
</package>
<package name="R1206">
<description>&lt;b&gt;RESISTOR&lt;/b&gt;</description>
<wire x1="0.9525" y1="-0.8128" x2="-0.9652" y2="-0.8128" width="0.1524" layer="51"/>
<wire x1="0.9525" y1="0.8128" x2="-0.9652" y2="0.8128" width="0.1524" layer="51"/>
<wire x1="-2.473" y1="0.983" x2="2.473" y2="0.983" width="0.0508" layer="39"/>
<wire x1="2.473" y1="0.983" x2="2.473" y2="-0.983" width="0.0508" layer="39"/>
<wire x1="2.473" y1="-0.983" x2="-2.473" y2="-0.983" width="0.0508" layer="39"/>
<wire x1="-2.473" y1="-0.983" x2="-2.473" y2="0.983" width="0.0508" layer="39"/>
<smd name="2" x="1.422" y="0" dx="1.6" dy="1.803" layer="1"/>
<smd name="1" x="-1.422" y="0" dx="1.6" dy="1.803" layer="1"/>
<text x="-1.27" y="1.27" size="1.27" layer="25">&gt;NAME</text>
<text x="-1.27" y="-2.54" size="1.27" layer="27">&gt;VALUE</text>
<rectangle x1="-1.6891" y1="-0.8763" x2="-0.9525" y2="0.8763" layer="51"/>
<rectangle x1="0.9525" y1="-0.8763" x2="1.6891" y2="0.8763" layer="51"/>
<rectangle x1="-0.3" y1="-0.7" x2="0.3" y2="0.7" layer="35"/>
</package>
<package name="R0603">
<description>&lt;b&gt;RESISTOR&lt;/b&gt;</description>
<wire x1="-0.432" y1="-0.356" x2="0.432" y2="-0.356" width="0.1524" layer="51"/>
<wire x1="0.432" y1="0.356" x2="-0.432" y2="0.356" width="0.1524" layer="51"/>
<wire x1="-1.473" y1="0.983" x2="1.473" y2="0.983" width="0.0508" layer="39"/>
<wire x1="1.473" y1="0.983" x2="1.473" y2="-0.983" width="0.0508" layer="39"/>
<wire x1="1.473" y1="-0.983" x2="-1.473" y2="-0.983" width="0.0508" layer="39"/>
<wire x1="-1.473" y1="-0.983" x2="-1.473" y2="0.983" width="0.0508" layer="39"/>
<smd name="1" x="-0.85" y="0" dx="1" dy="1.1" layer="1"/>
<smd name="2" x="0.85" y="0" dx="1" dy="1.1" layer="1"/>
<text x="-0.635" y="0.635" size="1.27" layer="25">&gt;NAME</text>
<text x="-0.635" y="-1.905" size="1.27" layer="27">&gt;VALUE</text>
<rectangle x1="0.4318" y1="-0.4318" x2="0.8382" y2="0.4318" layer="51"/>
<rectangle x1="-0.8382" y1="-0.4318" x2="-0.4318" y2="0.4318" layer="51"/>
<rectangle x1="-0.1999" y1="-0.4001" x2="0.1999" y2="0.4001" layer="35"/>
</package>
<package name="0207/10">
<description>&lt;b&gt;RESISTOR&lt;/b&gt;&lt;p&gt;
type 0207, grid 10 mm</description>
<wire x1="5.08" y1="0" x2="4.064" y2="0" width="0.6096" layer="51"/>
<wire x1="-5.08" y1="0" x2="-4.064" y2="0" width="0.6096" layer="51"/>
<wire x1="-3.175" y1="0.889" x2="-2.921" y2="1.143" width="0.1524" layer="21" curve="-90"/>
<wire x1="-3.175" y1="-0.889" x2="-2.921" y2="-1.143" width="0.1524" layer="21" curve="90"/>
<wire x1="2.921" y1="-1.143" x2="3.175" y2="-0.889" width="0.1524" layer="21" curve="90"/>
<wire x1="2.921" y1="1.143" x2="3.175" y2="0.889" width="0.1524" layer="21" curve="-90"/>
<wire x1="-3.175" y1="-0.889" x2="-3.175" y2="0.889" width="0.1524" layer="21"/>
<wire x1="-2.921" y1="1.143" x2="-2.54" y2="1.143" width="0.1524" layer="21"/>
<wire x1="-2.413" y1="1.016" x2="-2.54" y2="1.143" width="0.1524" layer="21"/>
<wire x1="-2.921" y1="-1.143" x2="-2.54" y2="-1.143" width="0.1524" layer="21"/>
<wire x1="-2.413" y1="-1.016" x2="-2.54" y2="-1.143" width="0.1524" layer="21"/>
<wire x1="2.413" y1="1.016" x2="2.54" y2="1.143" width="0.1524" layer="21"/>
<wire x1="2.413" y1="1.016" x2="-2.413" y2="1.016" width="0.1524" layer="21"/>
<wire x1="2.413" y1="-1.016" x2="2.54" y2="-1.143" width="0.1524" layer="21"/>
<wire x1="2.413" y1="-1.016" x2="-2.413" y2="-1.016" width="0.1524" layer="21"/>
<wire x1="2.921" y1="1.143" x2="2.54" y2="1.143" width="0.1524" layer="21"/>
<wire x1="2.921" y1="-1.143" x2="2.54" y2="-1.143" width="0.1524" layer="21"/>
<wire x1="3.175" y1="-0.889" x2="3.175" y2="0.889" width="0.1524" layer="21"/>
<pad name="1" x="-5.08" y="0" drill="0.8128" shape="octagon"/>
<pad name="2" x="5.08" y="0" drill="0.8128" shape="octagon"/>
<text x="-3.048" y="1.524" size="1.27" layer="25" ratio="10">&gt;NAME</text>
<text x="-2.2606" y="-0.635" size="1.27" layer="27" ratio="10">&gt;VALUE</text>
<rectangle x1="3.175" y1="-0.3048" x2="4.0386" y2="0.3048" layer="21"/>
<rectangle x1="-4.0386" y1="-0.3048" x2="-3.175" y2="0.3048" layer="21"/>
</package>
<package name="0207/2V">
<description>&lt;b&gt;RESISTOR&lt;/b&gt;&lt;p&gt;
type 0207, grid 2.5 mm</description>
<wire x1="-1.27" y1="0" x2="-0.381" y2="0" width="0.6096" layer="51"/>
<wire x1="-0.254" y1="0" x2="0.254" y2="0" width="0.6096" layer="21"/>
<wire x1="0.381" y1="0" x2="1.27" y2="0" width="0.6096" layer="51"/>
<circle x="-1.27" y="0" radius="1.27" width="0.1524" layer="21"/>
<circle x="-1.27" y="0" radius="1.016" width="0.1524" layer="51"/>
<pad name="1" x="-1.27" y="0" drill="0.8128" shape="octagon"/>
<pad name="2" x="1.27" y="0" drill="0.8128" shape="octagon"/>
<text x="-0.0508" y="1.016" size="1.27" layer="25" ratio="10">&gt;NAME</text>
<text x="-0.0508" y="-2.2352" size="1.27" layer="27" ratio="10">&gt;VALUE</text>
</package>
<package name="E5-10">
<description>&lt;b&gt;ELECTROLYTIC CAPACITOR&lt;/b&gt;&lt;p&gt;
grid 5.08 mm, diameter 10 mm</description>
<wire x1="-1.143" y1="0" x2="-0.889" y2="0" width="0.1524" layer="21"/>
<wire x1="-0.889" y1="0" x2="-0.889" y2="-1.143" width="0.1524" layer="21"/>
<wire x1="-0.889" y1="-1.143" x2="-0.254" y2="-1.143" width="0.1524" layer="21"/>
<wire x1="-0.254" y1="-1.143" x2="-0.254" y2="1.143" width="0.1524" layer="21"/>
<wire x1="-0.254" y1="1.143" x2="-0.889" y2="1.143" width="0.1524" layer="21"/>
<wire x1="-0.889" y1="1.143" x2="-0.889" y2="0" width="0.1524" layer="21"/>
<wire x1="0.635" y1="0" x2="1.143" y2="0" width="0.1524" layer="21"/>
<wire x1="-3.81" y1="1.651" x2="-3.81" y2="0.889" width="0.1524" layer="21"/>
<wire x1="-3.429" y1="1.27" x2="-4.191" y2="1.27" width="0.1524" layer="21"/>
<wire x1="1.143" y1="0" x2="1.651" y2="0" width="0.1524" layer="51"/>
<wire x1="-1.651" y1="0" x2="-1.143" y2="0" width="0.1524" layer="51"/>
<circle x="0" y="0" radius="5" width="0.1524" layer="21"/>
<pad name="+" x="-2.54" y="0" drill="1.016" diameter="2.54"/>
<pad name="-" x="2.54" y="0" drill="1.016" diameter="2.54" shape="octagon"/>
<text x="4.699" y="2.7432" size="1.27" layer="25" ratio="10">&gt;NAME</text>
<text x="-3.1242" y="-3.2258" size="1.27" layer="27" ratio="10">&gt;VALUE</text>
<rectangle x1="0.254" y1="-1.143" x2="0.889" y2="1.143" layer="21"/>
</package>
<package name="E2-5">
<description>&lt;b&gt;ELECTROLYTIC CAPACITOR&lt;/b&gt;&lt;p&gt;
grid 2.032 mm, diameter 5 mm</description>
<wire x1="-1.524" y1="0" x2="-0.762" y2="0" width="0.1524" layer="51"/>
<wire x1="-0.762" y1="0" x2="-0.762" y2="-1.016" width="0.1524" layer="51"/>
<wire x1="-0.762" y1="-1.016" x2="-0.254" y2="-1.016" width="0.1524" layer="51"/>
<wire x1="-0.254" y1="-1.016" x2="-0.254" y2="1.016" width="0.1524" layer="51"/>
<wire x1="-0.254" y1="1.016" x2="-0.762" y2="1.016" width="0.1524" layer="51"/>
<wire x1="-0.762" y1="1.016" x2="-0.762" y2="0" width="0.1524" layer="51"/>
<wire x1="0.635" y1="0" x2="1.524" y2="0" width="0.1524" layer="51"/>
<wire x1="-1.27" y1="1.778" x2="-0.762" y2="1.778" width="0.1524" layer="21"/>
<wire x1="-1.016" y1="1.524" x2="-1.016" y2="2.032" width="0.1524" layer="21"/>
<circle x="0" y="0" radius="2.54" width="0.1524" layer="21"/>
<pad name="-" x="1.016" y="0" drill="0.8128" diameter="1.27" shape="octagon"/>
<pad name="+" x="-1.016" y="0" drill="0.8128" diameter="1.27"/>
<text x="2.54" y="1.016" size="1.27" layer="25" ratio="10">&gt;NAME</text>
<text x="2.54" y="-2.159" size="1.27" layer="27" ratio="10">&gt;VALUE</text>
<rectangle x1="0.254" y1="-1.016" x2="0.762" y2="1.016" layer="51"/>
</package>
<package name="E2,5-6">
<description>&lt;b&gt;ELECTROLYTIC CAPACITOR&lt;/b&gt;&lt;p&gt;
grid 2.54 mm, diameter 6 mm</description>
<wire x1="-2.032" y1="1.27" x2="-1.651" y2="1.27" width="0.1524" layer="21"/>
<wire x1="-1.651" y1="0.889" x2="-1.651" y2="1.27" width="0.1524" layer="21"/>
<wire x1="-1.651" y1="1.27" x2="-1.27" y2="1.27" width="0.1524" layer="21"/>
<wire x1="-1.651" y1="1.27" x2="-1.651" y2="1.651" width="0.1524" layer="21"/>
<wire x1="-1.651" y1="0" x2="-0.762" y2="0" width="0.1524" layer="51"/>
<wire x1="-0.762" y1="0" x2="-0.762" y2="-1.27" width="0.1524" layer="51"/>
<wire x1="-0.762" y1="-1.27" x2="-0.254" y2="-1.27" width="0.1524" layer="51"/>
<wire x1="-0.254" y1="-1.27" x2="-0.254" y2="1.27" width="0.1524" layer="51"/>
<wire x1="-0.254" y1="1.27" x2="-0.762" y2="1.27" width="0.1524" layer="51"/>
<wire x1="-0.762" y1="1.27" x2="-0.762" y2="0" width="0.1524" layer="51"/>
<wire x1="0.635" y1="0" x2="1.651" y2="0" width="0.1524" layer="51"/>
<circle x="0" y="0" radius="2.794" width="0.1524" layer="21"/>
<pad name="-" x="1.27" y="0" drill="0.8128" diameter="1.6002" shape="octagon"/>
<pad name="+" x="-1.27" y="0" drill="0.8128" diameter="1.6002"/>
<text x="2.667" y="1.524" size="1.27" layer="25" ratio="10">&gt;NAME</text>
<text x="2.667" y="-2.667" size="1.27" layer="27" ratio="10">&gt;VALUE</text>
<rectangle x1="0.254" y1="-1.27" x2="0.762" y2="1.27" layer="51"/>
</package>
<package name="E1,8-4">
<description>&lt;b&gt;ELECTROLYTIC CAPACITOR&lt;/b&gt;&lt;p&gt;
grid 1.8 mm, diameter 4 mm</description>
<wire x1="-1.524" y1="0" x2="-0.762" y2="0" width="0.1524" layer="51"/>
<wire x1="-0.762" y1="0" x2="-0.762" y2="-1.016" width="0.1524" layer="51"/>
<wire x1="-0.762" y1="-1.016" x2="-0.254" y2="-1.016" width="0.1524" layer="51"/>
<wire x1="-0.254" y1="-1.016" x2="-0.254" y2="1.016" width="0.1524" layer="51"/>
<wire x1="-0.254" y1="1.016" x2="-0.762" y2="1.016" width="0.1524" layer="51"/>
<wire x1="-0.762" y1="1.016" x2="-0.762" y2="0" width="0.1524" layer="51"/>
<wire x1="0.635" y1="0" x2="1.524" y2="0" width="0.1524" layer="51"/>
<wire x1="-1.016" y1="1.397" x2="-0.508" y2="1.397" width="0.1524" layer="21"/>
<wire x1="-0.762" y1="1.143" x2="-0.762" y2="1.651" width="0.1524" layer="21"/>
<circle x="0" y="0" radius="2.032" width="0.1524" layer="21"/>
<pad name="-" x="0.889" y="0" drill="0.7112" diameter="1.1684" shape="octagon"/>
<pad name="+" x="-0.889" y="0" drill="0.7112" diameter="1.1684"/>
<text x="2.159" y="0.762" size="1.27" layer="25" ratio="10">&gt;NAME</text>
<text x="2.159" y="-1.778" size="1.27" layer="27" ratio="10">&gt;VALUE</text>
<rectangle x1="0.254" y1="-1.016" x2="0.762" y2="1.016" layer="51"/>
</package>
<package name="PANASONIC_D">
<description>&lt;b&gt;Panasonic Aluminium Electrolytic Capacitor VS-Serie Package D&lt;/b&gt;</description>
<wire x1="-3.25" y1="3.25" x2="1.55" y2="3.25" width="0.1016" layer="51"/>
<wire x1="1.55" y1="3.25" x2="3.25" y2="1.55" width="0.1016" layer="51"/>
<wire x1="3.25" y1="1.55" x2="3.25" y2="-1.55" width="0.1016" layer="51"/>
<wire x1="3.25" y1="-1.55" x2="1.55" y2="-3.25" width="0.1016" layer="51"/>
<wire x1="1.55" y1="-3.25" x2="-3.25" y2="-3.25" width="0.1016" layer="51"/>
<wire x1="-3.25" y1="-3.25" x2="-3.25" y2="3.25" width="0.1016" layer="51"/>
<wire x1="-3.25" y1="0.95" x2="-3.25" y2="3.25" width="0.1016" layer="21"/>
<wire x1="-3.25" y1="3.25" x2="1.55" y2="3.25" width="0.1016" layer="21"/>
<wire x1="1.55" y1="3.25" x2="3.25" y2="1.55" width="0.1016" layer="21"/>
<wire x1="3.25" y1="1.55" x2="3.25" y2="0.95" width="0.1016" layer="21"/>
<wire x1="3.25" y1="-0.95" x2="3.25" y2="-1.55" width="0.1016" layer="21"/>
<wire x1="3.25" y1="-1.55" x2="1.55" y2="-3.25" width="0.1016" layer="21"/>
<wire x1="1.55" y1="-3.25" x2="-3.25" y2="-3.25" width="0.1016" layer="21"/>
<wire x1="-3.25" y1="-3.25" x2="-3.25" y2="-0.95" width="0.1016" layer="21"/>
<wire x1="-2.95" y1="0.95" x2="2.95" y2="0.95" width="0.1016" layer="21" curve="-144.299363"/>
<wire x1="-2.95" y1="-0.95" x2="2.95" y2="-0.95" width="0.1016" layer="21" curve="144.299363"/>
<wire x1="-2.1" y1="2.25" x2="-2.1" y2="-2.2" width="0.1016" layer="51"/>
<circle x="0" y="0" radius="3.1" width="0.1016" layer="51"/>
<smd name="-" x="-2.4" y="0" dx="3" dy="1.4" layer="1"/>
<smd name="+" x="2.4" y="0" dx="3" dy="1.4" layer="1"/>
<text x="-1.75" y="1" size="1.016" layer="25">&gt;NAME</text>
<text x="-1.75" y="-1.975" size="1.016" layer="27">&gt;VALUE</text>
<rectangle x1="-3.65" y1="-0.35" x2="-3.05" y2="0.35" layer="51"/>
<rectangle x1="3.05" y1="-0.35" x2="3.65" y2="0.35" layer="51"/>
<polygon width="0.1016" layer="51">
<vertex x="-2.15" y="2.15"/>
<vertex x="-2.6" y="1.6"/>
<vertex x="-2.9" y="0.9"/>
<vertex x="-3.05" y="0"/>
<vertex x="-2.9" y="-0.95"/>
<vertex x="-2.55" y="-1.65"/>
<vertex x="-2.15" y="-2.15"/>
<vertex x="-2.15" y="2.1"/>
</polygon>
</package>
<package name="PANASONIC_C">
<description>&lt;b&gt;Panasonic Aluminium Electrolytic Capacitor VS-Serie Package C&lt;/b&gt;</description>
<wire x1="-2.6" y1="2.6" x2="1.25" y2="2.6" width="0.1016" layer="51"/>
<wire x1="1.25" y1="2.6" x2="2.6" y2="1.25" width="0.1016" layer="51"/>
<wire x1="2.6" y1="1.25" x2="2.6" y2="-1.25" width="0.1016" layer="51"/>
<wire x1="2.6" y1="-1.25" x2="1.25" y2="-2.6" width="0.1016" layer="51"/>
<wire x1="1.25" y1="-2.6" x2="-2.6" y2="-2.6" width="0.1016" layer="51"/>
<wire x1="-2.6" y1="-2.6" x2="-2.6" y2="2.6" width="0.1016" layer="51"/>
<wire x1="-2.6" y1="0.95" x2="-2.6" y2="2.6" width="0.1016" layer="21"/>
<wire x1="-2.6" y1="2.6" x2="1.25" y2="2.6" width="0.1016" layer="21"/>
<wire x1="1.25" y1="2.6" x2="2.6" y2="1.25" width="0.1016" layer="21"/>
<wire x1="2.6" y1="1.25" x2="2.6" y2="0.95" width="0.1016" layer="21"/>
<wire x1="2.6" y1="-0.95" x2="2.6" y2="-1.25" width="0.1016" layer="21"/>
<wire x1="2.6" y1="-1.25" x2="1.25" y2="-2.6" width="0.1016" layer="21"/>
<wire x1="1.25" y1="-2.6" x2="-2.6" y2="-2.6" width="0.1016" layer="21"/>
<wire x1="-2.6" y1="-2.6" x2="-2.6" y2="-0.95" width="0.1016" layer="21"/>
<wire x1="-2.3" y1="0.85" x2="2.3" y2="0.85" width="0.1016" layer="21" curve="-139.434882"/>
<wire x1="-2.3" y1="-0.85" x2="2.3" y2="-0.85" width="0.1016" layer="21" curve="139.434882"/>
<wire x1="-1.55" y1="1.85" x2="-1.55" y2="-1.85" width="0.1016" layer="51"/>
<circle x="0" y="0" radius="2.45" width="0.1016" layer="51"/>
<smd name="-" x="-2.05" y="0" dx="2.6" dy="1.4" layer="1"/>
<smd name="+" x="2.05" y="0" dx="2.6" dy="1.4" layer="1"/>
<text x="-2.6" y="2.75" size="1.016" layer="25">&gt;NAME</text>
<text x="-2.65" y="-3.775" size="1.016" layer="27">&gt;VALUE</text>
<rectangle x1="-2.95" y1="-0.35" x2="-2.4" y2="0.35" layer="51"/>
<rectangle x1="2.4" y1="-0.35" x2="2.95" y2="0.35" layer="51"/>
<polygon width="0.1016" layer="51">
<vertex x="-1.6" y="1.8"/>
<vertex x="-2" y="1.35"/>
<vertex x="-2.25" y="0.75"/>
<vertex x="-2.45" y="0.05"/>
<vertex x="-2.25" y="-0.75"/>
<vertex x="-1.95" y="-1.35"/>
<vertex x="-1.6" y="-1.8"/>
</polygon>
</package>
<package name="C0805">
<description>&lt;b&gt;CAPACITOR&lt;/b&gt;&lt;p&gt;</description>
<wire x1="-1.973" y1="0.983" x2="1.973" y2="0.983" width="0.0508" layer="39"/>
<wire x1="1.973" y1="-0.983" x2="-1.973" y2="-0.983" width="0.0508" layer="39"/>
<wire x1="-1.973" y1="-0.983" x2="-1.973" y2="0.983" width="0.0508" layer="39"/>
<wire x1="-0.381" y1="0.66" x2="0.381" y2="0.66" width="0.1016" layer="51"/>
<wire x1="-0.356" y1="-0.66" x2="0.381" y2="-0.66" width="0.1016" layer="51"/>
<wire x1="1.973" y1="0.983" x2="1.973" y2="-0.983" width="0.0508" layer="39"/>
<smd name="1" x="-0.95" y="0" dx="1.3" dy="1.5" layer="1"/>
<smd name="2" x="0.95" y="0" dx="1.3" dy="1.5" layer="1"/>
<text x="-1.27" y="1.27" size="1.27" layer="25">&gt;NAME</text>
<text x="-1.27" y="-2.54" size="1.27" layer="27">&gt;VALUE</text>
<rectangle x1="-1.0922" y1="-0.7239" x2="-0.3421" y2="0.7262" layer="51"/>
<rectangle x1="0.3556" y1="-0.7239" x2="1.1057" y2="0.7262" layer="51"/>
<rectangle x1="-0.1001" y1="-0.4001" x2="0.1001" y2="0.4001" layer="35"/>
</package>
<package name="C1206">
<description>&lt;b&gt;CAPACITOR&lt;/b&gt;</description>
<wire x1="-2.473" y1="0.983" x2="2.473" y2="0.983" width="0.0508" layer="39"/>
<wire x1="2.473" y1="-0.983" x2="-2.473" y2="-0.983" width="0.0508" layer="39"/>
<wire x1="-2.473" y1="-0.983" x2="-2.473" y2="0.983" width="0.0508" layer="39"/>
<wire x1="2.473" y1="0.983" x2="2.473" y2="-0.983" width="0.0508" layer="39"/>
<wire x1="-0.965" y1="0.787" x2="0.965" y2="0.787" width="0.1016" layer="51"/>
<wire x1="-0.965" y1="-0.787" x2="0.965" y2="-0.787" width="0.1016" layer="51"/>
<smd name="1" x="-1.4" y="0" dx="1.6" dy="1.8" layer="1"/>
<smd name="2" x="1.4" y="0" dx="1.6" dy="1.8" layer="1"/>
<text x="-1.27" y="1.27" size="1.27" layer="25">&gt;NAME</text>
<text x="-1.27" y="-2.54" size="1.27" layer="27">&gt;VALUE</text>
<rectangle x1="-1.7018" y1="-0.8509" x2="-0.9517" y2="0.8491" layer="51"/>
<rectangle x1="0.9517" y1="-0.8491" x2="1.7018" y2="0.8509" layer="51"/>
<rectangle x1="-0.1999" y1="-0.4001" x2="0.1999" y2="0.4001" layer="35"/>
</package>
<package name="C0603">
<description>&lt;b&gt;CAPACITOR&lt;/b&gt;</description>
<wire x1="-1.473" y1="0.983" x2="1.473" y2="0.983" width="0.0508" layer="39"/>
<wire x1="1.473" y1="0.983" x2="1.473" y2="-0.983" width="0.0508" layer="39"/>
<wire x1="1.473" y1="-0.983" x2="-1.473" y2="-0.983" width="0.0508" layer="39"/>
<wire x1="-1.473" y1="-0.983" x2="-1.473" y2="0.983" width="0.0508" layer="39"/>
<wire x1="-0.356" y1="0.432" x2="0.356" y2="0.432" width="0.1016" layer="51"/>
<wire x1="-0.356" y1="-0.419" x2="0.356" y2="-0.419" width="0.1016" layer="51"/>
<smd name="1" x="-0.85" y="0" dx="1.1" dy="1" layer="1"/>
<smd name="2" x="0.85" y="0" dx="1.1" dy="1" layer="1"/>
<text x="-0.635" y="0.635" size="1.27" layer="25">&gt;NAME</text>
<text x="-0.635" y="-1.905" size="1.27" layer="27">&gt;VALUE</text>
<rectangle x1="-0.8382" y1="-0.4699" x2="-0.3381" y2="0.4801" layer="51"/>
<rectangle x1="0.3302" y1="-0.4699" x2="0.8303" y2="0.4801" layer="51"/>
<rectangle x1="-0.1999" y1="-0.3" x2="0.1999" y2="0.3" layer="35"/>
</package>
<package name="C0504">
<description>&lt;b&gt;CAPACITOR&lt;/b&gt;</description>
<wire x1="-1.473" y1="0.983" x2="1.473" y2="0.983" width="0.0508" layer="39"/>
<wire x1="1.473" y1="0.983" x2="1.473" y2="-0.983" width="0.0508" layer="39"/>
<wire x1="1.473" y1="-0.983" x2="-1.473" y2="-0.983" width="0.0508" layer="39"/>
<wire x1="-1.473" y1="-0.983" x2="-1.473" y2="0.983" width="0.0508" layer="39"/>
<wire x1="-0.294" y1="0.559" x2="0.294" y2="0.559" width="0.1016" layer="51"/>
<wire x1="-0.294" y1="-0.559" x2="0.294" y2="-0.559" width="0.1016" layer="51"/>
<smd name="1" x="-0.7" y="0" dx="1" dy="1.3" layer="1"/>
<smd name="2" x="0.7" y="0" dx="1" dy="1.3" layer="1"/>
<text x="-0.635" y="1.27" size="1.27" layer="25">&gt;NAME</text>
<text x="-0.635" y="-2.54" size="1.27" layer="27">&gt;VALUE</text>
<rectangle x1="-0.6604" y1="-0.6223" x2="-0.2804" y2="0.6276" layer="51"/>
<rectangle x1="0.2794" y1="-0.6223" x2="0.6594" y2="0.6276" layer="51"/>
<rectangle x1="-0.1001" y1="-0.4001" x2="0.1001" y2="0.4001" layer="35"/>
</package>
<package name="C025-024X044">
<description>&lt;b&gt;CAPACITOR&lt;/b&gt;&lt;p&gt;
grid 2.5 mm, outline 2.4 x 4.4 mm</description>
<wire x1="-2.159" y1="-0.635" x2="-2.159" y2="0.635" width="0.1524" layer="21"/>
<wire x1="-2.159" y1="0.635" x2="-1.651" y2="1.143" width="0.1524" layer="21" curve="-90"/>
<wire x1="-2.159" y1="-0.635" x2="-1.651" y2="-1.143" width="0.1524" layer="21" curve="90"/>
<wire x1="1.651" y1="1.143" x2="-1.651" y2="1.143" width="0.1524" layer="21"/>
<wire x1="2.159" y1="-0.635" x2="2.159" y2="0.635" width="0.1524" layer="21"/>
<wire x1="1.651" y1="-1.143" x2="-1.651" y2="-1.143" width="0.1524" layer="21"/>
<wire x1="1.651" y1="1.143" x2="2.159" y2="0.635" width="0.1524" layer="21" curve="-90"/>
<wire x1="1.651" y1="-1.143" x2="2.159" y2="-0.635" width="0.1524" layer="21" curve="90"/>
<wire x1="-0.3048" y1="0.762" x2="-0.3048" y2="-0.762" width="0.3048" layer="21"/>
<wire x1="0.3302" y1="0.762" x2="0.3302" y2="-0.762" width="0.3048" layer="21"/>
<wire x1="1.27" y1="0" x2="0.3302" y2="0" width="0.1524" layer="51"/>
<wire x1="-1.27" y1="0" x2="-0.3048" y2="0" width="0.1524" layer="51"/>
<pad name="1" x="-1.27" y="0" drill="0.8128" shape="octagon"/>
<pad name="2" x="1.27" y="0" drill="0.8128" shape="octagon"/>
<text x="-1.778" y="1.397" size="1.27" layer="25" ratio="10">&gt;NAME</text>
<text x="-1.778" y="-2.667" size="1.27" layer="27" ratio="10">&gt;VALUE</text>
</package>
<package name="C025-025X050">
<description>&lt;b&gt;CAPACITOR&lt;/b&gt;&lt;p&gt;
grid 2.5 mm, outline 2.5 x 5 mm</description>
<wire x1="-2.159" y1="1.27" x2="2.159" y2="1.27" width="0.1524" layer="21"/>
<wire x1="2.159" y1="-1.27" x2="-2.159" y2="-1.27" width="0.1524" layer="21"/>
<wire x1="2.413" y1="1.016" x2="2.413" y2="-1.016" width="0.1524" layer="21"/>
<wire x1="-2.413" y1="1.016" x2="-2.413" y2="-1.016" width="0.1524" layer="21"/>
<wire x1="2.159" y1="1.27" x2="2.413" y2="1.016" width="0.1524" layer="21" curve="-90"/>
<wire x1="-2.413" y1="1.016" x2="-2.159" y2="1.27" width="0.1524" layer="21" curve="-90"/>
<wire x1="2.159" y1="-1.27" x2="2.413" y2="-1.016" width="0.1524" layer="21" curve="90"/>
<wire x1="-2.413" y1="-1.016" x2="-2.159" y2="-1.27" width="0.1524" layer="21" curve="90"/>
<wire x1="0.762" y1="0" x2="0.381" y2="0" width="0.1524" layer="51"/>
<wire x1="0.381" y1="0" x2="0.254" y2="0" width="0.1524" layer="21"/>
<wire x1="0.254" y1="0" x2="0.254" y2="0.762" width="0.254" layer="21"/>
<wire x1="0.254" y1="0" x2="0.254" y2="-0.762" width="0.254" layer="21"/>
<wire x1="-0.254" y1="0.762" x2="-0.254" y2="0" width="0.254" layer="21"/>
<wire x1="-0.254" y1="0" x2="-0.254" y2="-0.762" width="0.254" layer="21"/>
<wire x1="-0.254" y1="0" x2="-0.381" y2="0" width="0.1524" layer="21"/>
<wire x1="-0.381" y1="0" x2="-0.762" y2="0" width="0.1524" layer="51"/>
<pad name="1" x="-1.27" y="0" drill="0.8128" shape="octagon"/>
<pad name="2" x="1.27" y="0" drill="0.8128" shape="octagon"/>
<text x="-2.286" y="1.524" size="1.27" layer="25" ratio="10">&gt;NAME</text>
<text x="-2.286" y="-2.794" size="1.27" layer="27" ratio="10">&gt;VALUE</text>
</package>
<package name="C025-030X050">
<description>&lt;b&gt;CAPACITOR&lt;/b&gt;&lt;p&gt;
grid 2.5 mm, outline 3 x 5 mm</description>
<wire x1="-2.159" y1="1.524" x2="2.159" y2="1.524" width="0.1524" layer="21"/>
<wire x1="2.159" y1="-1.524" x2="-2.159" y2="-1.524" width="0.1524" layer="21"/>
<wire x1="2.413" y1="1.27" x2="2.413" y2="-1.27" width="0.1524" layer="21"/>
<wire x1="-2.413" y1="1.27" x2="-2.413" y2="-1.27" width="0.1524" layer="21"/>
<wire x1="2.159" y1="1.524" x2="2.413" y2="1.27" width="0.1524" layer="21" curve="-90"/>
<wire x1="-2.413" y1="1.27" x2="-2.159" y2="1.524" width="0.1524" layer="21" curve="-90"/>
<wire x1="2.159" y1="-1.524" x2="2.413" y2="-1.27" width="0.1524" layer="21" curve="90"/>
<wire x1="-2.413" y1="-1.27" x2="-2.159" y2="-1.524" width="0.1524" layer="21" curve="90"/>
<wire x1="0.762" y1="0" x2="0.381" y2="0" width="0.1524" layer="51"/>
<wire x1="0.381" y1="0" x2="0.254" y2="0" width="0.1524" layer="21"/>
<wire x1="0.254" y1="0" x2="0.254" y2="0.762" width="0.254" layer="21"/>
<wire x1="0.254" y1="0" x2="0.254" y2="-0.762" width="0.254" layer="21"/>
<wire x1="-0.254" y1="0.762" x2="-0.254" y2="0" width="0.254" layer="21"/>
<wire x1="-0.254" y1="0" x2="-0.254" y2="-0.762" width="0.254" layer="21"/>
<wire x1="-0.254" y1="0" x2="-0.381" y2="0" width="0.1524" layer="21"/>
<wire x1="-0.381" y1="0" x2="-0.762" y2="0" width="0.1524" layer="51"/>
<pad name="1" x="-1.27" y="0" drill="0.8128" shape="octagon"/>
<pad name="2" x="1.27" y="0" drill="0.8128" shape="octagon"/>
<text x="-2.286" y="1.905" size="1.27" layer="25" ratio="10">&gt;NAME</text>
<text x="-2.286" y="-3.048" size="1.27" layer="27" ratio="10">&gt;VALUE</text>
</package>
<package name="C025-040X050">
<description>&lt;b&gt;CAPACITOR&lt;/b&gt;&lt;p&gt;
grid 2.5 mm, outline 4 x 5 mm</description>
<wire x1="-2.159" y1="1.905" x2="2.159" y2="1.905" width="0.1524" layer="21"/>
<wire x1="2.159" y1="-1.905" x2="-2.159" y2="-1.905" width="0.1524" layer="21"/>
<wire x1="2.413" y1="1.651" x2="2.413" y2="-1.651" width="0.1524" layer="21"/>
<wire x1="-2.413" y1="1.651" x2="-2.413" y2="-1.651" width="0.1524" layer="21"/>
<wire x1="2.159" y1="1.905" x2="2.413" y2="1.651" width="0.1524" layer="21" curve="-90"/>
<wire x1="-2.413" y1="1.651" x2="-2.159" y2="1.905" width="0.1524" layer="21" curve="-90"/>
<wire x1="2.159" y1="-1.905" x2="2.413" y2="-1.651" width="0.1524" layer="21" curve="90"/>
<wire x1="-2.413" y1="-1.651" x2="-2.159" y2="-1.905" width="0.1524" layer="21" curve="90"/>
<wire x1="0.762" y1="0" x2="0.381" y2="0" width="0.1524" layer="51"/>
<wire x1="0.381" y1="0" x2="0.254" y2="0" width="0.1524" layer="21"/>
<wire x1="0.254" y1="0" x2="0.254" y2="0.762" width="0.254" layer="21"/>
<wire x1="0.254" y1="0" x2="0.254" y2="-0.762" width="0.254" layer="21"/>
<wire x1="-0.254" y1="0.762" x2="-0.254" y2="0" width="0.254" layer="21"/>
<wire x1="-0.254" y1="0" x2="-0.254" y2="-0.762" width="0.254" layer="21"/>
<wire x1="-0.254" y1="0" x2="-0.381" y2="0" width="0.1524" layer="21"/>
<wire x1="-0.381" y1="0" x2="-0.762" y2="0" width="0.1524" layer="51"/>
<pad name="1" x="-1.27" y="0" drill="0.8128" shape="octagon"/>
<pad name="2" x="1.27" y="0" drill="0.8128" shape="octagon"/>
<text x="-2.286" y="2.159" size="1.27" layer="25" ratio="10">&gt;NAME</text>
<text x="-2.286" y="-3.429" size="1.27" layer="27" ratio="10">&gt;VALUE</text>
</package>
<package name="C025-050X050">
<description>&lt;b&gt;CAPACITOR&lt;/b&gt;&lt;p&gt;
grid 2.5 mm, outline 5 x 5 mm</description>
<wire x1="-2.159" y1="2.286" x2="2.159" y2="2.286" width="0.1524" layer="21"/>
<wire x1="2.159" y1="-2.286" x2="-2.159" y2="-2.286" width="0.1524" layer="21"/>
<wire x1="2.413" y1="2.032" x2="2.413" y2="-2.032" width="0.1524" layer="21"/>
<wire x1="-2.413" y1="2.032" x2="-2.413" y2="-2.032" width="0.1524" layer="21"/>
<wire x1="2.159" y1="2.286" x2="2.413" y2="2.032" width="0.1524" layer="21" curve="-90"/>
<wire x1="-2.413" y1="2.032" x2="-2.159" y2="2.286" width="0.1524" layer="21" curve="-90"/>
<wire x1="2.159" y1="-2.286" x2="2.413" y2="-2.032" width="0.1524" layer="21" curve="90"/>
<wire x1="-2.413" y1="-2.032" x2="-2.159" y2="-2.286" width="0.1524" layer="21" curve="90"/>
<wire x1="0.762" y1="0" x2="0.381" y2="0" width="0.1524" layer="51"/>
<wire x1="0.381" y1="0" x2="0.254" y2="0" width="0.1524" layer="21"/>
<wire x1="0.254" y1="0" x2="0.254" y2="0.762" width="0.254" layer="21"/>
<wire x1="0.254" y1="0" x2="0.254" y2="-0.762" width="0.254" layer="21"/>
<wire x1="-0.254" y1="0.762" x2="-0.254" y2="0" width="0.254" layer="21"/>
<wire x1="-0.254" y1="0" x2="-0.254" y2="-0.762" width="0.254" layer="21"/>
<wire x1="-0.254" y1="0" x2="-0.381" y2="0" width="0.1524" layer="21"/>
<wire x1="-0.381" y1="0" x2="-0.762" y2="0" width="0.1524" layer="51"/>
<pad name="1" x="-1.27" y="0" drill="0.8128" shape="octagon"/>
<pad name="2" x="1.27" y="0" drill="0.8128" shape="octagon"/>
<text x="-2.286" y="2.54" size="1.27" layer="25" ratio="10">&gt;NAME</text>
<text x="-2.286" y="-3.81" size="1.27" layer="27" ratio="10">&gt;VALUE</text>
</package>
<package name="C025-060X050">
<description>&lt;b&gt;CAPACITOR&lt;/b&gt;&lt;p&gt;
grid 2.5 mm, outline 6 x 5 mm</description>
<wire x1="-2.159" y1="2.794" x2="2.159" y2="2.794" width="0.1524" layer="21"/>
<wire x1="2.159" y1="-2.794" x2="-2.159" y2="-2.794" width="0.1524" layer="21"/>
<wire x1="2.413" y1="2.54" x2="2.413" y2="-2.54" width="0.1524" layer="21"/>
<wire x1="-2.413" y1="2.54" x2="-2.413" y2="-2.54" width="0.1524" layer="21"/>
<wire x1="2.159" y1="2.794" x2="2.413" y2="2.54" width="0.1524" layer="21" curve="-90"/>
<wire x1="-2.413" y1="2.54" x2="-2.159" y2="2.794" width="0.1524" layer="21" curve="-90"/>
<wire x1="2.159" y1="-2.794" x2="2.413" y2="-2.54" width="0.1524" layer="21" curve="90"/>
<wire x1="-2.413" y1="-2.54" x2="-2.159" y2="-2.794" width="0.1524" layer="21" curve="90"/>
<wire x1="0.762" y1="0" x2="0.381" y2="0" width="0.1524" layer="51"/>
<wire x1="0.381" y1="0" x2="0.254" y2="0" width="0.1524" layer="21"/>
<wire x1="0.254" y1="0" x2="0.254" y2="0.762" width="0.254" layer="21"/>
<wire x1="0.254" y1="0" x2="0.254" y2="-0.762" width="0.254" layer="21"/>
<wire x1="-0.254" y1="0.762" x2="-0.254" y2="0" width="0.254" layer="21"/>
<wire x1="-0.254" y1="0" x2="-0.254" y2="-0.762" width="0.254" layer="21"/>
<wire x1="-0.254" y1="0" x2="-0.381" y2="0" width="0.1524" layer="21"/>
<wire x1="-0.381" y1="0" x2="-0.762" y2="0" width="0.1524" layer="51"/>
<pad name="1" x="-1.27" y="0" drill="0.8128" shape="octagon"/>
<pad name="2" x="1.27" y="0" drill="0.8128" shape="octagon"/>
<text x="-2.286" y="3.048" size="1.27" layer="25" ratio="10">&gt;NAME</text>
<text x="-2.032" y="-2.413" size="1.27" layer="27" ratio="10">&gt;VALUE</text>
</package>
<package name="C050-024X044">
<description>&lt;b&gt;CAPACITOR&lt;/b&gt;&lt;p&gt;
grid 5 mm, outline 2.4 x 4.4 mm</description>
<wire x1="-2.159" y1="-0.635" x2="-2.159" y2="0.635" width="0.1524" layer="51"/>
<wire x1="-2.159" y1="0.635" x2="-1.651" y2="1.143" width="0.1524" layer="21" curve="-90"/>
<wire x1="-2.159" y1="-0.635" x2="-1.651" y2="-1.143" width="0.1524" layer="21" curve="90"/>
<wire x1="1.651" y1="1.143" x2="-1.651" y2="1.143" width="0.1524" layer="21"/>
<wire x1="2.159" y1="-0.635" x2="2.159" y2="0.635" width="0.1524" layer="51"/>
<wire x1="1.651" y1="-1.143" x2="-1.651" y2="-1.143" width="0.1524" layer="21"/>
<wire x1="1.651" y1="1.143" x2="2.159" y2="0.635" width="0.1524" layer="21" curve="-90"/>
<wire x1="1.651" y1="-1.143" x2="2.159" y2="-0.635" width="0.1524" layer="21" curve="90"/>
<wire x1="-0.3048" y1="0.762" x2="-0.3048" y2="0" width="0.3048" layer="21"/>
<wire x1="-0.3048" y1="0" x2="-0.3048" y2="-0.762" width="0.3048" layer="21"/>
<wire x1="0.3302" y1="0.762" x2="0.3302" y2="0" width="0.3048" layer="21"/>
<wire x1="0.3302" y1="0" x2="0.3302" y2="-0.762" width="0.3048" layer="21"/>
<wire x1="1.27" y1="0" x2="0.3302" y2="0" width="0.1524" layer="21"/>
<wire x1="-1.27" y1="0" x2="-0.3048" y2="0" width="0.1524" layer="21"/>
<pad name="1" x="-2.54" y="0" drill="0.8128" shape="octagon"/>
<pad name="2" x="2.54" y="0" drill="0.8128" shape="octagon"/>
<text x="-2.159" y="1.397" size="1.27" layer="25" ratio="10">&gt;NAME</text>
<text x="-2.159" y="-2.667" size="1.27" layer="27" ratio="10">&gt;VALUE</text>
<rectangle x1="2.159" y1="-0.381" x2="2.54" y2="0.381" layer="51"/>
<rectangle x1="-2.54" y1="-0.381" x2="-2.159" y2="0.381" layer="51"/>
</package>
<package name="C050-025X075">
<description>&lt;b&gt;CAPACITOR&lt;/b&gt;&lt;p&gt;
grid 5 mm, outline 2.5 x 7.5 mm</description>
<wire x1="-0.3048" y1="0.635" x2="-0.3048" y2="0" width="0.3048" layer="21"/>
<wire x1="-0.3048" y1="0" x2="-0.3048" y2="-0.635" width="0.3048" layer="21"/>
<wire x1="-0.3048" y1="0" x2="-1.524" y2="0" width="0.1524" layer="21"/>
<wire x1="0.3302" y1="0.635" x2="0.3302" y2="0" width="0.3048" layer="21"/>
<wire x1="0.3302" y1="0" x2="0.3302" y2="-0.635" width="0.3048" layer="21"/>
<wire x1="0.3302" y1="0" x2="1.524" y2="0" width="0.1524" layer="21"/>
<wire x1="-3.683" y1="1.016" x2="-3.683" y2="-1.016" width="0.1524" layer="21"/>
<wire x1="-3.429" y1="-1.27" x2="3.429" y2="-1.27" width="0.1524" layer="21"/>
<wire x1="3.683" y1="-1.016" x2="3.683" y2="1.016" width="0.1524" layer="21"/>
<wire x1="3.429" y1="1.27" x2="-3.429" y2="1.27" width="0.1524" layer="21"/>
<wire x1="3.429" y1="1.27" x2="3.683" y2="1.016" width="0.1524" layer="21" curve="-90"/>
<wire x1="3.429" y1="-1.27" x2="3.683" y2="-1.016" width="0.1524" layer="21" curve="90"/>
<wire x1="-3.683" y1="-1.016" x2="-3.429" y2="-1.27" width="0.1524" layer="21" curve="90"/>
<wire x1="-3.683" y1="1.016" x2="-3.429" y2="1.27" width="0.1524" layer="21" curve="-90"/>
<pad name="1" x="-2.54" y="0" drill="0.8128" shape="octagon"/>
<pad name="2" x="2.54" y="0" drill="0.8128" shape="octagon"/>
<text x="-3.429" y="1.651" size="1.27" layer="25" ratio="10">&gt;NAME</text>
<text x="-3.429" y="-2.794" size="1.27" layer="27" ratio="10">&gt;VALUE</text>
</package>
<package name="C050-030X075">
<description>&lt;b&gt;CAPACITOR&lt;/b&gt;&lt;p&gt;
grid 5 mm, outline 3 x 7.5 mm</description>
<wire x1="-0.3048" y1="0.635" x2="-0.3048" y2="0" width="0.3048" layer="21"/>
<wire x1="-0.3048" y1="0" x2="-0.3048" y2="-0.635" width="0.3048" layer="21"/>
<wire x1="-0.3048" y1="0" x2="-1.524" y2="0" width="0.1524" layer="21"/>
<wire x1="0.3302" y1="0.635" x2="0.3302" y2="0" width="0.3048" layer="21"/>
<wire x1="0.3302" y1="0" x2="0.3302" y2="-0.635" width="0.3048" layer="21"/>
<wire x1="0.3302" y1="0" x2="1.524" y2="0" width="0.1524" layer="21"/>
<wire x1="-3.683" y1="1.27" x2="-3.683" y2="-1.27" width="0.1524" layer="21"/>
<wire x1="-3.429" y1="-1.524" x2="3.429" y2="-1.524" width="0.1524" layer="21"/>
<wire x1="3.683" y1="-1.27" x2="3.683" y2="1.27" width="0.1524" layer="21"/>
<wire x1="3.429" y1="1.524" x2="-3.429" y2="1.524" width="0.1524" layer="21"/>
<wire x1="3.429" y1="1.524" x2="3.683" y2="1.27" width="0.1524" layer="21" curve="-90"/>
<wire x1="3.429" y1="-1.524" x2="3.683" y2="-1.27" width="0.1524" layer="21" curve="90"/>
<wire x1="-3.683" y1="-1.27" x2="-3.429" y2="-1.524" width="0.1524" layer="21" curve="90"/>
<wire x1="-3.683" y1="1.27" x2="-3.429" y2="1.524" width="0.1524" layer="21" curve="-90"/>
<pad name="1" x="-2.54" y="0" drill="0.8128" shape="octagon"/>
<pad name="2" x="2.54" y="0" drill="0.8128" shape="octagon"/>
<text x="-3.556" y="1.905" size="1.27" layer="25" ratio="10">&gt;NAME</text>
<text x="-3.556" y="-3.048" size="1.27" layer="27" ratio="10">&gt;VALUE</text>
</package>
<package name="C050-035X075">
<description>&lt;b&gt;CAPACITOR&lt;/b&gt;&lt;p&gt;
grid 5 mm, outline 3.5 x 7.5 mm</description>
<wire x1="-0.3048" y1="0.635" x2="-0.3048" y2="0" width="0.3048" layer="21"/>
<wire x1="-0.3048" y1="0" x2="-0.3048" y2="-0.635" width="0.3048" layer="21"/>
<wire x1="-0.3048" y1="0" x2="-1.524" y2="0" width="0.1524" layer="21"/>
<wire x1="0.3302" y1="0.635" x2="0.3302" y2="0" width="0.3048" layer="21"/>
<wire x1="0.3302" y1="0" x2="0.3302" y2="-0.635" width="0.3048" layer="21"/>
<wire x1="0.3302" y1="0" x2="1.524" y2="0" width="0.1524" layer="21"/>
<wire x1="-3.683" y1="1.524" x2="-3.683" y2="-1.524" width="0.1524" layer="21"/>
<wire x1="-3.429" y1="-1.778" x2="3.429" y2="-1.778" width="0.1524" layer="21"/>
<wire x1="3.683" y1="-1.524" x2="3.683" y2="1.524" width="0.1524" layer="21"/>
<wire x1="3.429" y1="1.778" x2="-3.429" y2="1.778" width="0.1524" layer="21"/>
<wire x1="3.429" y1="1.778" x2="3.683" y2="1.524" width="0.1524" layer="21" curve="-90"/>
<wire x1="3.429" y1="-1.778" x2="3.683" y2="-1.524" width="0.1524" layer="21" curve="90"/>
<wire x1="-3.683" y1="-1.524" x2="-3.429" y2="-1.778" width="0.1524" layer="21" curve="90"/>
<wire x1="-3.683" y1="1.524" x2="-3.429" y2="1.778" width="0.1524" layer="21" curve="-90"/>
<pad name="1" x="-2.54" y="0" drill="0.8128" shape="octagon"/>
<pad name="2" x="2.54" y="0" drill="0.8128" shape="octagon"/>
<text x="-3.556" y="2.159" size="1.27" layer="25" ratio="10">&gt;NAME</text>
<text x="-3.556" y="-3.429" size="1.27" layer="27" ratio="10">&gt;VALUE</text>
</package>
<package name="C050-045X075">
<description>&lt;b&gt;CAPACITOR&lt;/b&gt;&lt;p&gt;
grid 5 mm, outline 4.5 x 7.5 mm</description>
<wire x1="-0.3048" y1="0.635" x2="-0.3048" y2="0" width="0.3048" layer="21"/>
<wire x1="-0.3048" y1="0" x2="-0.3048" y2="-0.635" width="0.3048" layer="21"/>
<wire x1="-0.3048" y1="0" x2="-1.524" y2="0" width="0.1524" layer="21"/>
<wire x1="0.3302" y1="0.635" x2="0.3302" y2="0" width="0.3048" layer="21"/>
<wire x1="0.3302" y1="0" x2="0.3302" y2="-0.635" width="0.3048" layer="21"/>
<wire x1="0.3302" y1="0" x2="1.524" y2="0" width="0.1524" layer="21"/>
<wire x1="-3.683" y1="2.032" x2="-3.683" y2="-2.032" width="0.1524" layer="21"/>
<wire x1="-3.429" y1="-2.286" x2="3.429" y2="-2.286" width="0.1524" layer="21"/>
<wire x1="3.683" y1="-2.032" x2="3.683" y2="2.032" width="0.1524" layer="21"/>
<wire x1="3.429" y1="2.286" x2="-3.429" y2="2.286" width="0.1524" layer="21"/>
<wire x1="3.429" y1="2.286" x2="3.683" y2="2.032" width="0.1524" layer="21" curve="-90"/>
<wire x1="3.429" y1="-2.286" x2="3.683" y2="-2.032" width="0.1524" layer="21" curve="90"/>
<wire x1="-3.683" y1="-2.032" x2="-3.429" y2="-2.286" width="0.1524" layer="21" curve="90"/>
<wire x1="-3.683" y1="2.032" x2="-3.429" y2="2.286" width="0.1524" layer="21" curve="-90"/>
<pad name="1" x="-2.54" y="0" drill="0.8128" shape="octagon"/>
<pad name="2" x="2.54" y="0" drill="0.8128" shape="octagon"/>
<text x="-3.556" y="2.667" size="1.27" layer="25" ratio="10">&gt;NAME</text>
<text x="-3.556" y="-3.81" size="1.27" layer="27" ratio="10">&gt;VALUE</text>
</package>
<package name="C050-050X075">
<description>&lt;b&gt;CAPACITOR&lt;/b&gt;&lt;p&gt;
grid 5 mm, outline 5 x 7.5 mm</description>
<wire x1="-0.3048" y1="0.635" x2="-0.3048" y2="0" width="0.3048" layer="21"/>
<wire x1="-0.3048" y1="0" x2="-0.3048" y2="-0.635" width="0.3048" layer="21"/>
<wire x1="-0.3048" y1="0" x2="-1.524" y2="0" width="0.1524" layer="21"/>
<wire x1="0.3302" y1="0.635" x2="0.3302" y2="0" width="0.3048" layer="21"/>
<wire x1="0.3302" y1="0" x2="0.3302" y2="-0.635" width="0.3048" layer="21"/>
<wire x1="0.3302" y1="0" x2="1.524" y2="0" width="0.1524" layer="21"/>
<wire x1="-3.683" y1="2.286" x2="-3.683" y2="-2.286" width="0.1524" layer="21"/>
<wire x1="-3.429" y1="-2.54" x2="3.429" y2="-2.54" width="0.1524" layer="21"/>
<wire x1="3.683" y1="-2.286" x2="3.683" y2="2.286" width="0.1524" layer="21"/>
<wire x1="3.429" y1="2.54" x2="-3.429" y2="2.54" width="0.1524" layer="21"/>
<wire x1="3.429" y1="2.54" x2="3.683" y2="2.286" width="0.1524" layer="21" curve="-90"/>
<wire x1="3.429" y1="-2.54" x2="3.683" y2="-2.286" width="0.1524" layer="21" curve="90"/>
<wire x1="-3.683" y1="-2.286" x2="-3.429" y2="-2.54" width="0.1524" layer="21" curve="90"/>
<wire x1="-3.683" y1="2.286" x2="-3.429" y2="2.54" width="0.1524" layer="21" curve="-90"/>
<pad name="1" x="-2.54" y="0" drill="0.8128" shape="octagon"/>
<pad name="2" x="2.54" y="0" drill="0.8128" shape="octagon"/>
<text x="-3.429" y="2.921" size="1.27" layer="25" ratio="10">&gt;NAME</text>
<text x="-3.175" y="-2.159" size="1.27" layer="27" ratio="10">&gt;VALUE</text>
</package>
<package name="C050-055X075">
<description>&lt;b&gt;CAPACITOR&lt;/b&gt;&lt;p&gt;
grid 5 mm, outline 5.5 x 7.5 mm</description>
<wire x1="-0.3048" y1="0.635" x2="-0.3048" y2="0" width="0.3048" layer="21"/>
<wire x1="-0.3048" y1="0" x2="-0.3048" y2="-0.635" width="0.3048" layer="21"/>
<wire x1="-0.3048" y1="0" x2="-1.524" y2="0" width="0.1524" layer="21"/>
<wire x1="0.3302" y1="0.635" x2="0.3302" y2="0" width="0.3048" layer="21"/>
<wire x1="0.3302" y1="0" x2="0.3302" y2="-0.635" width="0.3048" layer="21"/>
<wire x1="0.3302" y1="0" x2="1.524" y2="0" width="0.1524" layer="21"/>
<wire x1="-3.683" y1="2.54" x2="-3.683" y2="-2.54" width="0.1524" layer="21"/>
<wire x1="-3.429" y1="-2.794" x2="3.429" y2="-2.794" width="0.1524" layer="21"/>
<wire x1="3.683" y1="-2.54" x2="3.683" y2="2.54" width="0.1524" layer="21"/>
<wire x1="3.429" y1="2.794" x2="-3.429" y2="2.794" width="0.1524" layer="21"/>
<wire x1="3.429" y1="2.794" x2="3.683" y2="2.54" width="0.1524" layer="21" curve="-90"/>
<wire x1="3.429" y1="-2.794" x2="3.683" y2="-2.54" width="0.1524" layer="21" curve="90"/>
<wire x1="-3.683" y1="-2.54" x2="-3.429" y2="-2.794" width="0.1524" layer="21" curve="90"/>
<wire x1="-3.683" y1="2.54" x2="-3.429" y2="2.794" width="0.1524" layer="21" curve="-90"/>
<pad name="1" x="-2.54" y="0" drill="0.8128" shape="octagon"/>
<pad name="2" x="2.54" y="0" drill="0.8128" shape="octagon"/>
<text x="-3.429" y="3.175" size="1.27" layer="25" ratio="10">&gt;NAME</text>
<text x="-3.302" y="-2.286" size="1.27" layer="27" ratio="10">&gt;VALUE</text>
</package>
<package name="C050-075X075">
<description>&lt;b&gt;CAPACITOR&lt;/b&gt;&lt;p&gt;
grid 5 mm, outline 7.5 x 7.5 mm</description>
<wire x1="-1.524" y1="0" x2="-0.4572" y2="0" width="0.1524" layer="21"/>
<wire x1="-0.4572" y1="0" x2="-0.4572" y2="0.762" width="0.4064" layer="21"/>
<wire x1="-0.4572" y1="0" x2="-0.4572" y2="-0.762" width="0.4064" layer="21"/>
<wire x1="0.4318" y1="0.762" x2="0.4318" y2="0" width="0.4064" layer="21"/>
<wire x1="0.4318" y1="0" x2="1.524" y2="0" width="0.1524" layer="21"/>
<wire x1="0.4318" y1="0" x2="0.4318" y2="-0.762" width="0.4064" layer="21"/>
<wire x1="-3.683" y1="3.429" x2="-3.683" y2="-3.429" width="0.1524" layer="21"/>
<wire x1="-3.429" y1="-3.683" x2="3.429" y2="-3.683" width="0.1524" layer="21"/>
<wire x1="3.683" y1="-3.429" x2="3.683" y2="3.429" width="0.1524" layer="21"/>
<wire x1="3.429" y1="3.683" x2="-3.429" y2="3.683" width="0.1524" layer="21"/>
<wire x1="3.429" y1="3.683" x2="3.683" y2="3.429" width="0.1524" layer="21" curve="-90"/>
<wire x1="3.429" y1="-3.683" x2="3.683" y2="-3.429" width="0.1524" layer="21" curve="90"/>
<wire x1="-3.683" y1="-3.429" x2="-3.429" y2="-3.683" width="0.1524" layer="21" curve="90"/>
<wire x1="-3.683" y1="3.429" x2="-3.429" y2="3.683" width="0.1524" layer="21" curve="-90"/>
<pad name="1" x="-2.54" y="0" drill="0.8128" shape="octagon"/>
<pad name="2" x="2.54" y="0" drill="0.8128" shape="octagon"/>
<text x="-3.429" y="4.064" size="1.27" layer="25" ratio="10">&gt;NAME</text>
<text x="-3.175" y="-2.921" size="1.27" layer="27" ratio="10">&gt;VALUE</text>
</package>
</packages>
<symbols>
<symbol name="R-EU">
<wire x1="-2.54" y1="-0.889" x2="2.54" y2="-0.889" width="0.254" layer="94"/>
<wire x1="2.54" y1="0.889" x2="-2.54" y2="0.889" width="0.254" layer="94"/>
<wire x1="2.54" y1="-0.889" x2="2.54" y2="0.889" width="0.254" layer="94"/>
<wire x1="-2.54" y1="-0.889" x2="-2.54" y2="0.889" width="0.254" layer="94"/>
<text x="0" y="1.4986" size="1.778" layer="95" align="bottom-center">&gt;NAME</text>
<text x="0" y="-3.302" size="1.778" layer="96" align="bottom-center">&gt;VALUE</text>
<pin name="2" x="5.08" y="0" visible="off" length="short" direction="pas" swaplevel="1" rot="R180"/>
<pin name="1" x="-5.08" y="0" visible="off" length="short" direction="pas" swaplevel="1"/>
</symbol>
<symbol name="CPOL">
<wire x1="-1.524" y1="-0.889" x2="1.524" y2="-0.889" width="0.254" layer="94"/>
<wire x1="1.524" y1="-0.889" x2="1.524" y2="0" width="0.254" layer="94"/>
<wire x1="-1.524" y1="0" x2="-1.524" y2="-0.889" width="0.254" layer="94"/>
<wire x1="-1.524" y1="0" x2="1.524" y2="0" width="0.254" layer="94"/>
<text x="1.143" y="0.4826" size="1.778" layer="95">&gt;NAME</text>
<text x="-0.5842" y="0.4064" size="1.27" layer="94" rot="R90">+</text>
<text x="1.143" y="-4.5974" size="1.778" layer="96">&gt;VALUE</text>
<rectangle x1="-1.651" y1="-2.54" x2="1.651" y2="-1.651" layer="94"/>
<pin name="-" x="0" y="-5.08" visible="off" length="short" direction="pas" rot="R90"/>
<pin name="+" x="0" y="2.54" visible="off" length="short" direction="pas" rot="R270"/>
</symbol>
<symbol name="C-EU">
<wire x1="0" y1="0" x2="0" y2="-0.508" width="0.1524" layer="94"/>
<wire x1="0" y1="-2.54" x2="0" y2="-2.032" width="0.1524" layer="94"/>
<text x="1.524" y="0.381" size="1.778" layer="95">&gt;NAME</text>
<text x="1.524" y="-4.699" size="1.778" layer="96">&gt;VALUE</text>
<rectangle x1="-2.032" y1="-2.032" x2="2.032" y2="-1.524" layer="94"/>
<rectangle x1="-2.032" y1="-1.016" x2="2.032" y2="-0.508" layer="94"/>
<pin name="1" x="0" y="2.54" visible="off" length="short" direction="pas" swaplevel="1" rot="R270"/>
<pin name="2" x="0" y="-5.08" visible="off" length="short" direction="pas" swaplevel="1" rot="R90"/>
</symbol>
</symbols>
<devicesets>
<deviceset name="R" prefix="R" uservalue="yes">
<description>&lt;b&gt;Resistor&lt;/b&gt;

&lt;p&gt;This is officially the most boring component ever.&lt;/p&gt;</description>
<gates>
<gate name="G$1" symbol="R-EU" x="0" y="0"/>
</gates>
<devices>
<device name="0805" package="R0805">
<connects>
<connect gate="G$1" pin="1" pad="1"/>
<connect gate="G$1" pin="2" pad="2"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="1206" package="R1206">
<connects>
<connect gate="G$1" pin="1" pad="1"/>
<connect gate="G$1" pin="2" pad="2"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="0603" package="R0603">
<connects>
<connect gate="G$1" pin="1" pad="1"/>
<connect gate="G$1" pin="2" pad="2"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="T0207" package="0207/10">
<connects>
<connect gate="G$1" pin="1" pad="1"/>
<connect gate="G$1" pin="2" pad="2"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="T0207V" package="0207/2V">
<connects>
<connect gate="G$1" pin="1" pad="1"/>
<connect gate="G$1" pin="2" pad="2"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
</devices>
</deviceset>
<deviceset name="CPOL" prefix="C" uservalue="yes">
<description>&lt;b&gt;Polarized Capacitor&lt;/b&gt;

&lt;p&gt;May come in the electrolytic and tantalum variants&lt;/p&gt;</description>
<gates>
<gate name="G$1" symbol="CPOL" x="0" y="0"/>
</gates>
<devices>
<device name="-E5-10" package="E5-10">
<connects>
<connect gate="G$1" pin="+" pad="+"/>
<connect gate="G$1" pin="-" pad="-"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="-E2-5" package="E2-5">
<connects>
<connect gate="G$1" pin="+" pad="+"/>
<connect gate="G$1" pin="-" pad="-"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="-E2.5-6" package="E2,5-6">
<connects>
<connect gate="G$1" pin="+" pad="+"/>
<connect gate="G$1" pin="-" pad="-"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="-E1.8-4" package="E1,8-4">
<connects>
<connect gate="G$1" pin="+" pad="+"/>
<connect gate="G$1" pin="-" pad="-"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="-MCVVT016M101EA1L" package="PANASONIC_D">
<connects>
<connect gate="G$1" pin="+" pad="+"/>
<connect gate="G$1" pin="-" pad="-"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="-MCVVT016M470DA1L" package="PANASONIC_C">
<connects>
<connect gate="G$1" pin="+" pad="+"/>
<connect gate="G$1" pin="-" pad="-"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
</devices>
</deviceset>
<deviceset name="C" prefix="C" uservalue="yes">
<description>&lt;b&gt;Ceramic Capacitor&lt;/b&gt;

&lt;p&gt;Those little yellow/brown things&lt;/p&gt;</description>
<gates>
<gate name="G$1" symbol="C-EU" x="0" y="0"/>
</gates>
<devices>
<device name="0805" package="C0805">
<connects>
<connect gate="G$1" pin="1" pad="1"/>
<connect gate="G$1" pin="2" pad="2"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="1206" package="C1206">
<connects>
<connect gate="G$1" pin="1" pad="1"/>
<connect gate="G$1" pin="2" pad="2"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="0603" package="C0603">
<connects>
<connect gate="G$1" pin="1" pad="1"/>
<connect gate="G$1" pin="2" pad="2"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="0504" package="C0504">
<connects>
<connect gate="G$1" pin="1" pad="1"/>
<connect gate="G$1" pin="2" pad="2"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="2.5-2.4X4.4" package="C025-024X044">
<connects>
<connect gate="G$1" pin="1" pad="1"/>
<connect gate="G$1" pin="2" pad="2"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="2.5-2.5X5.0" package="C025-025X050">
<connects>
<connect gate="G$1" pin="1" pad="1"/>
<connect gate="G$1" pin="2" pad="2"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="2.5-3.0X5.0" package="C025-030X050">
<connects>
<connect gate="G$1" pin="1" pad="1"/>
<connect gate="G$1" pin="2" pad="2"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="2.5-4.0X5.0" package="C025-040X050">
<connects>
<connect gate="G$1" pin="1" pad="1"/>
<connect gate="G$1" pin="2" pad="2"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="2.5-5.0X5.0" package="C025-050X050">
<connects>
<connect gate="G$1" pin="1" pad="1"/>
<connect gate="G$1" pin="2" pad="2"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="2.5-6.0X5.0" package="C025-060X050">
<connects>
<connect gate="G$1" pin="1" pad="1"/>
<connect gate="G$1" pin="2" pad="2"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="5.0-2.4X4.4" package="C050-024X044">
<connects>
<connect gate="G$1" pin="1" pad="1"/>
<connect gate="G$1" pin="2" pad="2"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="5.0-2.5X7.5" package="C050-025X075">
<connects>
<connect gate="G$1" pin="1" pad="1"/>
<connect gate="G$1" pin="2" pad="2"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="5.0-3.0X7.5" package="C050-030X075">
<connects>
<connect gate="G$1" pin="1" pad="1"/>
<connect gate="G$1" pin="2" pad="2"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="5.0-3.5X7.5" package="C050-035X075">
<connects>
<connect gate="G$1" pin="1" pad="1"/>
<connect gate="G$1" pin="2" pad="2"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="5.0-4.5X7.5" package="C050-045X075">
<connects>
<connect gate="G$1" pin="1" pad="1"/>
<connect gate="G$1" pin="2" pad="2"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="5.0-5.0X7.5" package="C050-050X075">
<connects>
<connect gate="G$1" pin="1" pad="1"/>
<connect gate="G$1" pin="2" pad="2"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="5.0-5.5X7.5" package="C050-055X075">
<connects>
<connect gate="G$1" pin="1" pad="1"/>
<connect gate="G$1" pin="2" pad="2"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="5.0-7.5X7.5" package="C050-075X075">
<connects>
<connect gate="G$1" pin="1" pad="1"/>
<connect gate="G$1" pin="2" pad="2"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
</devices>
</deviceset>
</devicesets>
</library>
<library name="innove-supply">
<description>&lt;b&gt;Supply Symbols&lt;/b&gt;

&lt;p&gt;GND, VCC, 0V, +5V, -5V, etc.&lt;/p&gt;

&lt;p&gt;Please keep in mind, that these devices are necessary for the automatic wiring of the supply signals.&lt;/p&gt;

&lt;p&gt;The pin name defined in the symbol is identical to the net which is to be wired automatically.&lt;/p&gt;

&lt;p&gt;In this library the device names are the same as the pin names of the symbols, therefore the correct signal names appear next to the supply symbols in the schematic.&lt;/p&gt;

&lt;p&gt;&lt;author&gt;Created by Nathan Campos &amp;lt;nathan@innoveworkshop.com&amp;gt;&lt;/author&gt;&lt;/p&gt;</description>
<packages>
</packages>
<symbols>
<symbol name="GND">
<wire x1="-1.905" y1="0" x2="1.905" y2="0" width="0.254" layer="94"/>
<text x="0" y="-2.54" size="1.778" layer="96" align="bottom-center">&gt;VALUE</text>
<pin name="GND" x="0" y="2.54" visible="off" length="short" direction="sup" rot="R270"/>
</symbol>
<symbol name="+12V">
<wire x1="1.27" y1="-1.905" x2="0" y2="0" width="0.254" layer="94"/>
<wire x1="0" y1="0" x2="-1.27" y2="-1.905" width="0.254" layer="94"/>
<text x="0" y="2.54" size="1.778" layer="96" rot="R180" align="bottom-center">&gt;VALUE</text>
<pin name="+3V3" x="0" y="-2.54" visible="off" length="short" direction="sup" rot="R90"/>
</symbol>
</symbols>
<devicesets>
<deviceset name="GND" prefix="GND">
<description>&lt;b&gt;SUPPLY SYMBOL&lt;/b&gt;</description>
<gates>
<gate name="1" symbol="GND" x="0" y="0"/>
</gates>
<devices>
<device name="">
<technologies>
<technology name=""/>
</technologies>
</device>
</devices>
</deviceset>
<deviceset name="+12V" prefix="+12V">
<description>&lt;b&gt;SUPPLY SYMBOL&lt;/b&gt;</description>
<gates>
<gate name="G$1" symbol="+12V" x="0" y="0"/>
</gates>
<devices>
<device name="">
<technologies>
<technology name=""/>
</technologies>
</device>
</devices>
</deviceset>
</devicesets>
</library>
<library name="innove-con-jst-xh">
<description>&lt;b&gt;JST Connectors XH Series&lt;/b&gt;
&lt;p&gt;
&lt;author&gt;Created by yuhki50@gmail.com&lt;/author&gt;
&lt;/p&gt;</description>
<packages>
<package name="B2B-XH-A">
<wire x1="-3.7" y1="2.35" x2="-3.7" y2="1.55" width="0.1524" layer="21"/>
<wire x1="-3.7" y1="1.55" x2="-3.7" y2="0.25" width="0.1524" layer="21"/>
<wire x1="-3.7" y1="0.25" x2="-3.7" y2="-3.4" width="0.1524" layer="21"/>
<wire x1="-3.7" y1="2.35" x2="-1.47" y2="2.35" width="0.1524" layer="21"/>
<wire x1="-1.47" y1="2.35" x2="1.47" y2="2.35" width="0.1524" layer="21"/>
<wire x1="1.47" y1="2.35" x2="3.7" y2="2.35" width="0.1524" layer="21"/>
<wire x1="3.7" y1="-3.4" x2="-3.7" y2="-3.4" width="0.1524" layer="21"/>
<pad name="1" x="-1.25" y="0" drill="1.016" shape="square" rot="R90"/>
<pad name="2" x="1.25" y="0" drill="1.016" rot="R90"/>
<text x="-3.7" y="3.5" size="1.27" layer="25" ratio="10">&gt;NAME</text>
<text x="-3.7" y="-5.5" size="1.27" layer="27">&gt;VALUE</text>
<wire x1="3.7" y1="2.35" x2="3.7" y2="1.55" width="0.1524" layer="21"/>
<wire x1="3.7" y1="1.55" x2="3.7" y2="0.25" width="0.1524" layer="21"/>
<wire x1="3.7" y1="0.25" x2="3.7" y2="-3.4" width="0.1524" layer="21"/>
<wire x1="-3.7" y1="0.25" x2="-3.175" y2="0.25" width="0.1524" layer="21"/>
<wire x1="-3.175" y1="0.25" x2="-3.175" y2="-2.875" width="0.1524" layer="21"/>
<wire x1="-3.175" y1="-2.875" x2="3.175" y2="-2.875" width="0.1524" layer="21"/>
<wire x1="3.175" y1="-2.875" x2="3.175" y2="0.25" width="0.1524" layer="21"/>
<wire x1="3.175" y1="0.25" x2="3.7" y2="0.25" width="0.1524" layer="21"/>
<wire x1="-3.7" y1="1.55" x2="-1.47" y2="1.55" width="0.1524" layer="21"/>
<wire x1="-1.47" y1="1.55" x2="-1.47" y2="2.35" width="0.1524" layer="21"/>
<wire x1="1.47" y1="2.35" x2="1.47" y2="1.55" width="0.1524" layer="21"/>
<wire x1="1.47" y1="1.55" x2="3.7" y2="1.55" width="0.1524" layer="21"/>
</package>
</packages>
<symbols>
<symbol name="CON-1X2">
<wire x1="-3.81" y1="-5.08" x2="3.81" y2="-5.08" width="0.4064" layer="94"/>
<wire x1="3.81" y1="-5.08" x2="3.81" y2="2.54" width="0.4064" layer="94"/>
<wire x1="3.81" y1="2.54" x2="-3.81" y2="2.54" width="0.4064" layer="94"/>
<wire x1="-3.81" y1="2.54" x2="-3.81" y2="-5.08" width="0.4064" layer="94"/>
<text x="-3.81" y="3.81" size="1.778" layer="95">&gt;NAME</text>
<text x="-3.81" y="-7.62" size="1.778" layer="96">&gt;VALUE</text>
<pin name="1" x="0" y="0" visible="pad" length="short" direction="pas" function="dot"/>
<pin name="2" x="0" y="-2.54" visible="pad" length="short" direction="pas" function="dot"/>
</symbol>
</symbols>
<devicesets>
<deviceset name="B2B-XH-A" prefix="CN" uservalue="yes">
<description>&lt;b&gt;Wire-to-Board 2.5mm pitch top mount connector&lt;/b&gt;
&lt;p&gt;
Source:
&lt;ul&gt;
&lt;li&gt;http://www.jst-mfg.com/product/pdf/jpn/XH.pdf&lt;/li&gt;
&lt;li&gt;http://www.jst-mfg.com/product/detail.php?series=277&lt;/li&gt;
&lt;ul&gt;
&lt;/p&gt;</description>
<gates>
<gate name="G$1" symbol="CON-1X2" x="0" y="0"/>
</gates>
<devices>
<device name="" package="B2B-XH-A">
<connects>
<connect gate="G$1" pin="1" pad="1"/>
<connect gate="G$1" pin="2" pad="2"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
</devices>
</deviceset>
</devicesets>
</library>
<library name="innove-leds">
<description>&lt;b&gt;Light Emitting Diodes&lt;/b&gt;

&lt;p&gt;Special diodes that emit photons when current passes through them.&lt;/p&gt;

&lt;p&gt;&lt;author&gt;Created by Nathan Campos &amp;lt;nathan@innoveworkshop.com&amp;gt;&lt;/author&gt;&lt;/p&gt;</description>
<packages>
<package name="1206">
<description>&lt;b&gt;CHICAGO MINIATURE LAMP, INC.&lt;/b&gt;&lt;p&gt;
7022X Series SMT LEDs 1206 Package Size</description>
<wire x1="1.55" y1="-0.75" x2="-1.55" y2="-0.75" width="0.1016" layer="51"/>
<wire x1="-1.55" y1="-0.75" x2="-1.55" y2="0.75" width="0.1016" layer="51"/>
<wire x1="-1.55" y1="0.75" x2="1.55" y2="0.75" width="0.1016" layer="51"/>
<wire x1="1.55" y1="0.75" x2="1.55" y2="-0.75" width="0.1016" layer="51"/>
<wire x1="-0.55" y1="-0.5" x2="0.55" y2="-0.5" width="0.1016" layer="21" curve="95.452622"/>
<wire x1="-0.55" y1="-0.5" x2="-0.55" y2="0.5" width="0.1016" layer="51" curve="-84.547378"/>
<wire x1="-0.55" y1="0.5" x2="0.55" y2="0.5" width="0.1016" layer="21" curve="-95.452622"/>
<wire x1="0.55" y1="0.5" x2="0.55" y2="-0.5" width="0.1016" layer="51" curve="-84.547378"/>
<smd name="A" x="-1.422" y="0" dx="1.6" dy="1.803" layer="1"/>
<smd name="C" x="1.422" y="0" dx="1.6" dy="1.803" layer="1"/>
<text x="-1.27" y="1.27" size="1.27" layer="25">&gt;NAME</text>
<text x="-1.27" y="-2.54" size="1.27" layer="27">&gt;VALUE</text>
<rectangle x1="-0.1" y1="-0.1" x2="0.1" y2="0.1" layer="21"/>
<rectangle x1="0.45" y1="-0.7" x2="0.8" y2="-0.45" layer="51"/>
<rectangle x1="0.8" y1="-0.7" x2="0.9" y2="0.5" layer="51"/>
<rectangle x1="0.8" y1="0.55" x2="0.9" y2="0.7" layer="51"/>
<rectangle x1="-0.9" y1="-0.7" x2="-0.8" y2="0.5" layer="51"/>
<rectangle x1="-0.9" y1="0.55" x2="-0.8" y2="0.7" layer="51"/>
<rectangle x1="0.45" y1="-0.7" x2="0.6" y2="-0.45" layer="21"/>
</package>
<package name="LD260">
<description>&lt;B&gt;LED&lt;/B&gt;&lt;p&gt;
5 mm, square, Siemens</description>
<wire x1="-1.27" y1="-1.27" x2="0" y2="-1.27" width="0.1524" layer="21"/>
<wire x1="0" y1="-1.27" x2="1.27" y2="-1.27" width="0.1524" layer="21"/>
<wire x1="1.27" y1="1.27" x2="0" y2="1.27" width="0.1524" layer="21"/>
<wire x1="0" y1="1.27" x2="-1.27" y2="1.27" width="0.1524" layer="21"/>
<wire x1="1.27" y1="-1.27" x2="1.27" y2="-0.889" width="0.1524" layer="21"/>
<wire x1="1.27" y1="1.27" x2="1.27" y2="0.889" width="0.1524" layer="21"/>
<wire x1="1.27" y1="0.889" x2="1.27" y2="0" width="0.1524" layer="51"/>
<wire x1="1.27" y1="0" x2="1.27" y2="-0.889" width="0.1524" layer="51"/>
<wire x1="-1.27" y1="1.27" x2="-1.27" y2="0.889" width="0.1524" layer="21"/>
<wire x1="-1.27" y1="-1.27" x2="-1.27" y2="-0.889" width="0.1524" layer="21"/>
<wire x1="-1.27" y1="-0.889" x2="-1.27" y2="0" width="0.1524" layer="51"/>
<wire x1="-1.27" y1="0" x2="-1.27" y2="0.889" width="0.1524" layer="51"/>
<wire x1="0" y1="1.27" x2="0.9917" y2="0.7934" width="0.1524" layer="21" curve="-51.33923"/>
<wire x1="-0.9917" y1="0.7934" x2="0" y2="1.27" width="0.1524" layer="21" curve="-51.33923"/>
<wire x1="0" y1="-1.27" x2="0.9917" y2="-0.7934" width="0.1524" layer="21" curve="51.33923"/>
<wire x1="-0.9917" y1="-0.7934" x2="0" y2="-1.27" width="0.1524" layer="21" curve="51.33923"/>
<wire x1="0.9558" y1="-0.8363" x2="1.27" y2="0" width="0.1524" layer="51" curve="41.185419"/>
<wire x1="0.9756" y1="0.813" x2="1.2699" y2="0" width="0.1524" layer="51" curve="-39.806332"/>
<wire x1="-1.27" y1="0" x2="-0.9643" y2="-0.8265" width="0.1524" layer="51" curve="40.600331"/>
<wire x1="-1.27" y1="0" x2="-0.9643" y2="0.8265" width="0.1524" layer="51" curve="-40.600331"/>
<wire x1="-0.889" y1="0" x2="0" y2="0.889" width="0.1524" layer="51" curve="-90"/>
<wire x1="-0.508" y1="0" x2="0" y2="0.508" width="0.1524" layer="51" curve="-90"/>
<wire x1="0" y1="-0.508" x2="0.508" y2="0" width="0.1524" layer="21" curve="90"/>
<wire x1="0" y1="-0.889" x2="0.889" y2="0" width="0.1524" layer="51" curve="90"/>
<pad name="A" x="-1.27" y="0" drill="0.8128" shape="octagon"/>
<pad name="K" x="1.27" y="0" drill="0.8128" shape="octagon"/>
<text x="-1.2954" y="1.4732" size="1.016" layer="25" ratio="10">&gt;NAME</text>
<text x="-1.27" y="-2.4892" size="1.016" layer="27" ratio="10">&gt;VALUE</text>
<rectangle x1="1.27" y1="-0.635" x2="2.032" y2="0.635" layer="51"/>
<rectangle x1="1.905" y1="-0.635" x2="2.032" y2="0.635" layer="21"/>
</package>
<package name="LED2X5">
<description>&lt;B&gt;LED&lt;/B&gt;&lt;p&gt;
2 x 5 mm, rectangle</description>
<wire x1="-2.54" y1="-1.27" x2="2.54" y2="-1.27" width="0.1524" layer="21"/>
<wire x1="2.54" y1="1.27" x2="2.54" y2="-1.27" width="0.1524" layer="21"/>
<wire x1="2.54" y1="1.27" x2="-2.54" y2="1.27" width="0.1524" layer="21"/>
<wire x1="-2.54" y1="-1.27" x2="-2.54" y2="1.27" width="0.1524" layer="21"/>
<wire x1="-1.905" y1="0" x2="0.508" y2="0" width="0.1524" layer="51"/>
<wire x1="-0.508" y1="0.381" x2="-0.508" y2="-0.381" width="0.1524" layer="51"/>
<wire x1="-0.508" y1="0.381" x2="0.508" y2="0" width="0.1524" layer="51"/>
<wire x1="0.508" y1="0" x2="1.778" y2="0" width="0.1524" layer="51"/>
<wire x1="0.508" y1="0" x2="-0.508" y2="-0.381" width="0.1524" layer="51"/>
<wire x1="0.508" y1="0.381" x2="0.508" y2="0" width="0.1524" layer="51"/>
<wire x1="0.508" y1="0" x2="0.508" y2="-0.381" width="0.1524" layer="51"/>
<wire x1="0.889" y1="-0.254" x2="1.143" y2="-0.762" width="0.1524" layer="51"/>
<wire x1="1.143" y1="-0.762" x2="1.143" y2="-0.508" width="0.1524" layer="51"/>
<wire x1="1.143" y1="-0.762" x2="0.9398" y2="-0.6096" width="0.1524" layer="51"/>
<wire x1="0.9398" y1="-0.6096" x2="1.143" y2="-0.508" width="0.1524" layer="51"/>
<wire x1="1.397" y1="-0.254" x2="1.651" y2="-0.762" width="0.1524" layer="51"/>
<wire x1="1.651" y1="-0.762" x2="1.651" y2="-0.508" width="0.1524" layer="51"/>
<wire x1="1.651" y1="-0.762" x2="1.4478" y2="-0.6096" width="0.1524" layer="51"/>
<wire x1="1.4478" y1="-0.6096" x2="1.651" y2="-0.508" width="0.1524" layer="51"/>
<pad name="A" x="-1.27" y="0" drill="0.8128" shape="octagon"/>
<pad name="K" x="1.27" y="0" drill="0.8128" shape="octagon"/>
<text x="-2.54" y="1.397" size="1.016" layer="25" ratio="10">&gt;NAME</text>
<text x="-2.54" y="-2.413" size="1.016" layer="27" ratio="10">&gt;VALUE</text>
<rectangle x1="2.159" y1="-1.27" x2="2.413" y2="1.27" layer="21"/>
</package>
<package name="LED3MM">
<description>&lt;B&gt;LED&lt;/B&gt;&lt;p&gt;
3 mm, round</description>
<wire x1="1.5748" y1="-1.27" x2="1.5748" y2="1.27" width="0.254" layer="51"/>
<wire x1="-1.524" y1="0" x2="-1.1708" y2="0.9756" width="0.1524" layer="51" curve="-39.80361"/>
<wire x1="-1.524" y1="0" x2="-1.1391" y2="-1.0125" width="0.1524" layer="51" curve="41.633208"/>
<wire x1="1.1571" y1="0.9918" x2="1.524" y2="0" width="0.1524" layer="51" curve="-40.601165"/>
<wire x1="1.1708" y1="-0.9756" x2="1.524" y2="0" width="0.1524" layer="51" curve="39.80361"/>
<wire x1="0" y1="1.524" x2="1.2401" y2="0.8858" width="0.1524" layer="21" curve="-54.461337"/>
<wire x1="-1.2192" y1="0.9144" x2="0" y2="1.524" width="0.1524" layer="21" curve="-53.130102"/>
<wire x1="0" y1="-1.524" x2="1.203" y2="-0.9356" width="0.1524" layer="21" curve="52.126876"/>
<wire x1="-1.203" y1="-0.9356" x2="0" y2="-1.524" width="0.1524" layer="21" curve="52.126876"/>
<wire x1="-0.635" y1="0" x2="0" y2="0.635" width="0.1524" layer="51" curve="-90"/>
<wire x1="-1.016" y1="0" x2="0" y2="1.016" width="0.1524" layer="51" curve="-90"/>
<wire x1="0" y1="-0.635" x2="0.635" y2="0" width="0.1524" layer="51" curve="90"/>
<wire x1="0" y1="-1.016" x2="1.016" y2="0" width="0.1524" layer="51" curve="90"/>
<wire x1="0" y1="2.032" x2="1.561" y2="1.3009" width="0.254" layer="21" curve="-50.193108"/>
<wire x1="-1.7929" y1="0.9562" x2="0" y2="2.032" width="0.254" layer="21" curve="-61.926949"/>
<wire x1="0" y1="-2.032" x2="1.5512" y2="-1.3126" width="0.254" layer="21" curve="49.763022"/>
<wire x1="-1.7643" y1="-1.0082" x2="0" y2="-2.032" width="0.254" layer="21" curve="60.255215"/>
<wire x1="-2.032" y1="0" x2="-1.7891" y2="0.9634" width="0.254" layer="51" curve="-28.301701"/>
<wire x1="-2.032" y1="0" x2="-1.7306" y2="-1.065" width="0.254" layer="51" curve="31.60822"/>
<pad name="A" x="-1.27" y="0" drill="0.8128" shape="octagon"/>
<pad name="K" x="1.27" y="0" drill="0.8128" shape="octagon"/>
<text x="1.905" y="0.381" size="1.27" layer="25" ratio="10">&gt;NAME</text>
<text x="1.905" y="-1.651" size="1.27" layer="27" ratio="10">&gt;VALUE</text>
</package>
<package name="LED5MM">
<description>&lt;B&gt;LED&lt;/B&gt;&lt;p&gt;
5 mm, round</description>
<wire x1="2.54" y1="-1.905" x2="2.54" y2="1.905" width="0.2032" layer="21"/>
<wire x1="2.54" y1="-1.905" x2="2.54" y2="1.905" width="0.254" layer="21" curve="-286.260205"/>
<wire x1="-1.143" y1="0" x2="0" y2="1.143" width="0.1524" layer="51" curve="-90"/>
<wire x1="0" y1="-1.143" x2="1.143" y2="0" width="0.1524" layer="51" curve="90"/>
<wire x1="-1.651" y1="0" x2="0" y2="1.651" width="0.1524" layer="51" curve="-90"/>
<wire x1="0" y1="-1.651" x2="1.651" y2="0" width="0.1524" layer="51" curve="90"/>
<wire x1="-2.159" y1="0" x2="0" y2="2.159" width="0.1524" layer="51" curve="-90"/>
<wire x1="0" y1="-2.159" x2="2.159" y2="0" width="0.1524" layer="51" curve="90"/>
<circle x="0" y="0" radius="2.54" width="0.1524" layer="21"/>
<pad name="A" x="-1.27" y="0" drill="0.8128" shape="octagon"/>
<pad name="K" x="1.27" y="0" drill="0.8128" shape="octagon"/>
<text x="3.175" y="0.5334" size="1.27" layer="25" ratio="10">&gt;NAME</text>
<text x="3.2004" y="-1.8034" size="1.27" layer="27" ratio="10">&gt;VALUE</text>
</package>
<package name="LSU260">
<description>&lt;B&gt;LED&lt;/B&gt;&lt;p&gt;
1 mm, round, Siemens</description>
<wire x1="0" y1="-0.508" x2="-1.143" y2="-0.508" width="0.1524" layer="51"/>
<wire x1="-1.143" y1="-0.508" x2="-1.143" y2="-0.254" width="0.1524" layer="51"/>
<wire x1="-1.143" y1="0.508" x2="0" y2="0.508" width="0.1524" layer="51"/>
<wire x1="-1.143" y1="-0.254" x2="-1.397" y2="-0.254" width="0.1524" layer="51"/>
<wire x1="-1.143" y1="-0.254" x2="-1.143" y2="0.254" width="0.1524" layer="51"/>
<wire x1="-1.397" y1="-0.254" x2="-1.397" y2="0.254" width="0.1524" layer="51"/>
<wire x1="-1.397" y1="0.254" x2="-1.143" y2="0.254" width="0.1524" layer="51"/>
<wire x1="-1.143" y1="0.254" x2="-1.143" y2="0.508" width="0.1524" layer="51"/>
<wire x1="0.508" y1="-0.254" x2="1.397" y2="-0.254" width="0.1524" layer="51"/>
<wire x1="1.397" y1="-0.254" x2="1.397" y2="0.254" width="0.1524" layer="51"/>
<wire x1="1.397" y1="0.254" x2="0.508" y2="0.254" width="0.1524" layer="51"/>
<wire x1="0.381" y1="-0.381" x2="0.254" y2="-0.508" width="0.1524" layer="21"/>
<wire x1="0.254" y1="-0.508" x2="-0.254" y2="-0.508" width="0.1524" layer="21"/>
<wire x1="-0.381" y1="-0.381" x2="-0.254" y2="-0.508" width="0.1524" layer="21"/>
<wire x1="0.381" y1="0.381" x2="0.254" y2="0.508" width="0.1524" layer="21"/>
<wire x1="0.254" y1="0.508" x2="-0.254" y2="0.508" width="0.1524" layer="21"/>
<wire x1="-0.381" y1="0.381" x2="-0.254" y2="0.508" width="0.1524" layer="21"/>
<wire x1="0" y1="-0.254" x2="0.254" y2="0" width="0.1524" layer="21" curve="90"/>
<wire x1="-0.254" y1="0" x2="0" y2="0.254" width="0.1524" layer="21" curve="-90"/>
<wire x1="0.381" y1="-0.381" x2="0.381" y2="0.381" width="0.1524" layer="21" curve="90"/>
<circle x="0" y="0" radius="0.508" width="0.1524" layer="51"/>
<pad name="A" x="-1.27" y="0" drill="0.8128" shape="octagon"/>
<pad name="K" x="1.27" y="0" drill="0.8128" shape="octagon"/>
<text x="-1.2954" y="0.8382" size="1.016" layer="25" ratio="10">&gt;NAME</text>
<text x="-1.27" y="-1.8542" size="1.016" layer="27" ratio="10">&gt;VALUE</text>
<rectangle x1="-1.397" y1="-0.254" x2="-1.143" y2="0.254" layer="51"/>
<rectangle x1="0.508" y1="-0.254" x2="1.397" y2="0.254" layer="51"/>
</package>
<package name="LZR181">
<description>&lt;B&gt;LED BLOCK&lt;/B&gt;&lt;p&gt;
1 LED, Siemens</description>
<wire x1="-1.27" y1="-1.27" x2="1.27" y2="-1.27" width="0.1524" layer="21"/>
<wire x1="1.27" y1="-1.27" x2="1.27" y2="-0.889" width="0.1524" layer="21"/>
<wire x1="1.27" y1="1.27" x2="1.27" y2="0.889" width="0.1524" layer="21"/>
<wire x1="1.27" y1="0.889" x2="1.27" y2="-0.889" width="0.1524" layer="51"/>
<wire x1="-1.27" y1="1.27" x2="-1.27" y2="0.889" width="0.1524" layer="21"/>
<wire x1="-1.27" y1="-1.27" x2="-1.27" y2="-0.889" width="0.1524" layer="21"/>
<wire x1="-1.27" y1="-0.889" x2="-1.27" y2="0.889" width="0.1524" layer="51"/>
<wire x1="-0.889" y1="0" x2="0" y2="0.889" width="0.1524" layer="51" curve="-90"/>
<wire x1="-0.508" y1="0" x2="0" y2="0.508" width="0.1524" layer="51" curve="-90"/>
<wire x1="0" y1="-0.508" x2="0.508" y2="0" width="0.1524" layer="21" curve="90"/>
<wire x1="0" y1="-0.889" x2="0.889" y2="0" width="0.1524" layer="51" curve="90"/>
<wire x1="-0.8678" y1="0.7439" x2="0" y2="1.143" width="0.1524" layer="21" curve="-49.396139"/>
<wire x1="0" y1="1.143" x2="0.8678" y2="0.7439" width="0.1524" layer="21" curve="-49.396139"/>
<wire x1="-0.8678" y1="-0.7439" x2="0" y2="-1.143" width="0.1524" layer="21" curve="49.396139"/>
<wire x1="0" y1="-1.143" x2="0.8678" y2="-0.7439" width="0.1524" layer="21" curve="49.396139"/>
<wire x1="0.8678" y1="0.7439" x2="1.143" y2="0" width="0.1524" layer="51" curve="-40.604135"/>
<wire x1="0.8678" y1="-0.7439" x2="1.143" y2="0" width="0.1524" layer="51" curve="40.604135"/>
<wire x1="-1.143" y1="0" x2="-0.8678" y2="0.7439" width="0.1524" layer="51" curve="-40.604135"/>
<wire x1="-1.143" y1="0" x2="-0.8678" y2="-0.7439" width="0.1524" layer="51" curve="40.604135"/>
<wire x1="-1.27" y1="1.27" x2="1.27" y2="1.27" width="0.1524" layer="21"/>
<pad name="A" x="-1.27" y="0" drill="0.8128" shape="octagon"/>
<pad name="K" x="1.27" y="0" drill="0.8128" shape="octagon"/>
<text x="-1.2954" y="1.4732" size="1.016" layer="25" ratio="10">&gt;NAME</text>
<text x="-1.27" y="-2.4892" size="1.016" layer="27" ratio="10">&gt;VALUE</text>
<rectangle x1="1.27" y1="-0.889" x2="1.524" y2="0.254" layer="51"/>
<rectangle x1="-1.524" y1="-0.254" x2="-1.27" y2="0.254" layer="51"/>
</package>
<package name="Q62902-B152">
<description>&lt;b&gt;LED HOLDER&lt;/b&gt;&lt;p&gt;
Siemens</description>
<wire x1="-2.9718" y1="-1.8542" x2="-2.9718" y2="-0.254" width="0.1524" layer="21"/>
<wire x1="-2.9718" y1="-0.254" x2="-2.9718" y2="0.254" width="0.1524" layer="21"/>
<wire x1="-2.9718" y1="0.254" x2="-2.9718" y2="1.8542" width="0.1524" layer="21"/>
<wire x1="2.9718" y1="-1.8542" x2="-2.1082" y2="-1.8542" width="0.1524" layer="21"/>
<wire x1="-2.1082" y1="-1.8542" x2="-2.54" y2="-1.8542" width="0.1524" layer="21"/>
<wire x1="-2.54" y1="1.8542" x2="-2.1082" y2="1.8542" width="0.1524" layer="21"/>
<wire x1="-2.1082" y1="1.8542" x2="2.9718" y2="1.8542" width="0.1524" layer="21"/>
<wire x1="-2.54" y1="-1.8542" x2="-2.54" y2="1.8542" width="0.1524" layer="21"/>
<wire x1="-2.54" y1="-1.8542" x2="-2.9718" y2="-1.8542" width="0.1524" layer="21"/>
<wire x1="-2.54" y1="1.8542" x2="-2.9718" y2="1.8542" width="0.1524" layer="21"/>
<wire x1="-2.9718" y1="0.254" x2="-2.9718" y2="-0.254" width="0.1524" layer="21" curve="180"/>
<wire x1="-1.1486" y1="0.8814" x2="0" y2="1.4478" width="0.1524" layer="21" curve="-52.498642"/>
<wire x1="0" y1="1.4478" x2="1.1476" y2="0.8827" width="0.1524" layer="21" curve="-52.433716"/>
<wire x1="-1.1351" y1="-0.8987" x2="0" y2="-1.4478" width="0.1524" layer="21" curve="51.629985"/>
<wire x1="0" y1="-1.4478" x2="1.1305" y2="-0.9044" width="0.1524" layer="21" curve="51.339172"/>
<wire x1="1.1281" y1="-0.9074" x2="1.4478" y2="0" width="0.1524" layer="51" curve="38.811177"/>
<wire x1="1.1401" y1="0.8923" x2="1.4478" y2="0" width="0.1524" layer="51" curve="-38.048073"/>
<wire x1="-1.4478" y1="0" x2="-1.1305" y2="-0.9044" width="0.1524" layer="51" curve="38.659064"/>
<wire x1="-1.4478" y1="0" x2="-1.1456" y2="0.8853" width="0.1524" layer="51" curve="-37.696376"/>
<wire x1="0" y1="1.7018" x2="1.4674" y2="0.8618" width="0.1524" layer="21" curve="-59.573488"/>
<wire x1="-1.4618" y1="0.8714" x2="0" y2="1.7018" width="0.1524" layer="21" curve="-59.200638"/>
<wire x1="0" y1="-1.7018" x2="1.4571" y2="-0.8793" width="0.1524" layer="21" curve="58.891781"/>
<wire x1="-1.4571" y1="-0.8793" x2="0" y2="-1.7018" width="0.1524" layer="21" curve="58.891781"/>
<wire x1="-1.7018" y1="0" x2="-1.4447" y2="0.8995" width="0.1524" layer="51" curve="-31.907626"/>
<wire x1="-1.7018" y1="0" x2="-1.4502" y2="-0.8905" width="0.1524" layer="51" curve="31.551992"/>
<wire x1="1.4521" y1="0.8874" x2="1.7018" y2="0" width="0.1524" layer="51" curve="-31.429586"/>
<wire x1="1.4459" y1="-0.8975" x2="1.7018" y2="0" width="0.1524" layer="51" curve="31.828757"/>
<wire x1="-2.1082" y1="1.8542" x2="-2.1082" y2="-1.8542" width="0.1524" layer="21"/>
<wire x1="-0.635" y1="0" x2="0" y2="0.635" width="0.1524" layer="21" curve="-90"/>
<wire x1="-1.016" y1="0" x2="0" y2="1.016" width="0.1524" layer="51" curve="-90"/>
<wire x1="0" y1="-0.635" x2="0.635" y2="0" width="0.1524" layer="51" curve="90"/>
<wire x1="0.0539" y1="-1.0699" x2="1.0699" y2="-0.0539" width="0.1524" layer="51" curve="90"/>
<wire x1="2.9718" y1="1.8542" x2="2.9718" y2="-1.8542" width="0.1524" layer="21"/>
<pad name="K" x="-1.27" y="0" drill="0.8128" shape="long" rot="R90"/>
<pad name="A" x="1.27" y="0" drill="0.8128" shape="long" rot="R90"/>
<text x="-1.905" y="2.286" size="1.27" layer="25" ratio="10">&gt;NAME</text>
<text x="-1.905" y="-3.556" size="1.27" layer="27" ratio="10">&gt;VALUE</text>
</package>
<package name="Q62902-B153">
<description>&lt;b&gt;LED HOLDER&lt;/b&gt;&lt;p&gt;
Siemens</description>
<wire x1="-5.5118" y1="-3.5052" x2="-5.5118" y2="-0.254" width="0.1524" layer="21"/>
<wire x1="-5.5118" y1="-0.254" x2="-5.5118" y2="0.254" width="0.1524" layer="21"/>
<wire x1="-5.5118" y1="0.254" x2="-5.5118" y2="3.5052" width="0.1524" layer="21"/>
<wire x1="5.5118" y1="-3.5052" x2="-4.6482" y2="-3.5052" width="0.1524" layer="21"/>
<wire x1="-4.6482" y1="-3.5052" x2="-5.08" y2="-3.5052" width="0.1524" layer="21"/>
<wire x1="-5.08" y1="3.5052" x2="-4.6482" y2="3.5052" width="0.1524" layer="21"/>
<wire x1="-4.6482" y1="3.5052" x2="5.5118" y2="3.5052" width="0.1524" layer="21"/>
<wire x1="-5.08" y1="-3.5052" x2="-5.08" y2="3.5052" width="0.1524" layer="21"/>
<wire x1="-5.08" y1="-3.5052" x2="-5.5118" y2="-3.5052" width="0.1524" layer="21"/>
<wire x1="-5.08" y1="3.5052" x2="-5.5118" y2="3.5052" width="0.1524" layer="21"/>
<wire x1="-5.5118" y1="0.254" x2="-5.5118" y2="-0.254" width="0.1524" layer="21" curve="180"/>
<wire x1="-4.6482" y1="3.5052" x2="-4.6482" y2="-3.5052" width="0.1524" layer="21"/>
<wire x1="5.5118" y1="3.5052" x2="5.5118" y2="-3.5052" width="0.1524" layer="21"/>
<wire x1="-2.54" y1="1.905" x2="-2.54" y2="-1.905" width="0.254" layer="21"/>
<wire x1="0" y1="-1.143" x2="1.143" y2="0" width="0.1524" layer="51" curve="90"/>
<wire x1="-1.143" y1="0" x2="0" y2="1.143" width="0.1524" layer="51" curve="-90"/>
<wire x1="0" y1="-1.651" x2="1.651" y2="0" width="0.1524" layer="51" curve="90"/>
<wire x1="-1.651" y1="0" x2="0" y2="1.651" width="0.1524" layer="51" curve="-90"/>
<wire x1="0" y1="-2.159" x2="2.159" y2="0" width="0.1524" layer="51" curve="90"/>
<wire x1="-2.2129" y1="0.0539" x2="-0.0539" y2="2.2129" width="0.1524" layer="51" curve="-90.010616"/>
<circle x="0" y="0" radius="2.54" width="0.1524" layer="21"/>
<circle x="0" y="0" radius="3.175" width="0.254" layer="21"/>
<pad name="A" x="1.27" y="0" drill="0.8128" shape="long" rot="R90"/>
<pad name="K" x="-1.27" y="0" drill="0.8128" shape="long" rot="R90"/>
<text x="-4.191" y="3.937" size="1.27" layer="25" ratio="10">&gt;NAME</text>
<text x="-4.318" y="-5.08" size="1.27" layer="27" ratio="10">&gt;VALUE</text>
</package>
<package name="Q62902-B155">
<description>&lt;b&gt;LED HOLDER&lt;/b&gt;&lt;p&gt;
Siemens</description>
<wire x1="-1.27" y1="-3.048" x2="-1.27" y2="-2.54" width="0.1524" layer="21"/>
<wire x1="10.033" y1="3.048" x2="2.921" y2="3.048" width="0.1524" layer="21"/>
<wire x1="10.033" y1="3.048" x2="10.033" y2="-3.048" width="0.1524" layer="21"/>
<wire x1="-1.27" y1="-3.048" x2="2.921" y2="-3.048" width="0.1524" layer="21"/>
<wire x1="2.921" y1="-3.048" x2="2.921" y2="3.048" width="0.1524" layer="21"/>
<wire x1="2.921" y1="-3.048" x2="10.033" y2="-3.048" width="0.1524" layer="21"/>
<wire x1="2.921" y1="3.048" x2="-1.27" y2="3.048" width="0.1524" layer="21"/>
<wire x1="-1.27" y1="2.54" x2="-5.207" y2="2.54" width="0.1524" layer="21"/>
<wire x1="-1.27" y1="2.54" x2="-1.27" y2="3.048" width="0.1524" layer="21"/>
<wire x1="-5.207" y1="-2.54" x2="-1.27" y2="-2.54" width="0.1524" layer="21"/>
<wire x1="-1.27" y1="-2.54" x2="-1.27" y2="2.54" width="0.1524" layer="21"/>
<wire x1="-5.207" y1="2.54" x2="-5.207" y2="-2.54" width="0.1524" layer="21" curve="180"/>
<wire x1="-6.985" y1="0.635" x2="-6.985" y2="-0.635" width="0.1524" layer="21"/>
<wire x1="-6.096" y1="1.397" x2="-6.096" y2="-1.397" width="0.1524" layer="21"/>
<wire x1="-5.207" y1="1.905" x2="-5.207" y2="-1.905" width="0.1524" layer="21"/>
<pad name="K" x="7.62" y="1.27" drill="0.8128" shape="long"/>
<pad name="A" x="7.62" y="-1.27" drill="0.8128" shape="long"/>
<text x="3.302" y="-2.794" size="1.016" layer="21" ratio="14">A+</text>
<text x="3.302" y="1.778" size="1.016" layer="21" ratio="14">K-</text>
<text x="11.684" y="-2.794" size="1.27" layer="25" ratio="10" rot="R90">&gt;NAME</text>
<text x="0.635" y="-4.445" size="1.27" layer="27" ratio="10">&gt;VALUE</text>
<rectangle x1="2.921" y1="1.016" x2="6.731" y2="1.524" layer="21"/>
<rectangle x1="2.921" y1="-1.524" x2="6.731" y2="-1.016" layer="21"/>
<hole x="0" y="0" drill="0.8128"/>
</package>
<package name="Q62902-B156">
<description>&lt;b&gt;LED HOLDER&lt;/b&gt;&lt;p&gt;
Siemens</description>
<wire x1="2.54" y1="-1.905" x2="2.54" y2="1.905" width="0.1524" layer="21"/>
<wire x1="-1.143" y1="0" x2="0" y2="1.143" width="0.1524" layer="51" curve="-90"/>
<wire x1="0" y1="-1.143" x2="1.143" y2="0" width="0.1524" layer="51" curve="90"/>
<wire x1="-1.651" y1="0" x2="0" y2="1.651" width="0.1524" layer="51" curve="-90"/>
<wire x1="0" y1="-1.651" x2="1.651" y2="0" width="0.1524" layer="51" curve="90"/>
<wire x1="-2.159" y1="0" x2="0" y2="2.159" width="0.1524" layer="51" curve="-90"/>
<wire x1="0.0539" y1="-2.2129" x2="2.2129" y2="-0.0539" width="0.1524" layer="51" curve="90.005308"/>
<wire x1="2.54" y1="3.81" x2="3.81" y2="2.54" width="0.1524" layer="21"/>
<wire x1="2.54" y1="3.81" x2="-3.81" y2="3.81" width="0.1524" layer="21"/>
<wire x1="-3.81" y1="-3.81" x2="-3.81" y2="3.81" width="0.1524" layer="21"/>
<wire x1="3.81" y1="2.54" x2="3.81" y2="-3.81" width="0.1524" layer="21"/>
<wire x1="-3.81" y1="-3.81" x2="-2.54" y2="-3.81" width="0.1524" layer="21"/>
<wire x1="-2.54" y1="-3.302" x2="-2.54" y2="-3.81" width="0.1524" layer="21"/>
<wire x1="3.81" y1="-3.81" x2="2.54" y2="-3.81" width="0.1524" layer="21"/>
<wire x1="2.54" y1="-3.302" x2="2.54" y2="-3.81" width="0.1524" layer="21"/>
<wire x1="2.54" y1="-3.302" x2="-2.54" y2="-3.302" width="0.1524" layer="21"/>
<circle x="0" y="0" radius="2.54" width="0.1524" layer="21"/>
<circle x="0" y="0" radius="3.175" width="0.254" layer="21"/>
<pad name="A" x="-1.27" y="0" drill="1.016" shape="long" rot="R90"/>
<pad name="K" x="1.27" y="0" drill="1.016" shape="long" rot="R90"/>
<text x="-3.81" y="4.0894" size="1.27" layer="25" ratio="10">&gt;NAME</text>
<text x="-3.7846" y="-5.3594" size="1.27" layer="27" ratio="10">&gt;VALUE</text>
<text x="-3.556" y="-3.302" size="1.016" layer="21" ratio="14">+</text>
<text x="2.794" y="-3.302" size="1.016" layer="21" ratio="14">-</text>
</package>
<package name="SFH480">
<description>&lt;B&gt;IR LED&lt;/B&gt;&lt;p&gt;
infrared emitting diode, Infineon
TO-18, lead spacing 2.54 mm, cathode marking&lt;p&gt;
Inifineon</description>
<wire x1="-2.159" y1="1.524" x2="-2.794" y2="2.159" width="0.1524" layer="21"/>
<wire x1="-2.794" y1="2.159" x2="-2.159" y2="2.794" width="0.1524" layer="21"/>
<wire x1="-1.524" y1="2.159" x2="-2.159" y2="2.794" width="0.1524" layer="21"/>
<wire x1="0" y1="1.778" x2="1.5358" y2="0.8959" width="0.1524" layer="21" curve="-59.743278"/>
<wire x1="-1.5358" y1="0.8959" x2="0" y2="1.778" width="0.1524" layer="21" curve="-59.743278"/>
<wire x1="-1.5358" y1="-0.8959" x2="0" y2="-1.778" width="0.1524" layer="21" curve="59.743278"/>
<wire x1="0" y1="-1.778" x2="1.5358" y2="-0.8959" width="0.1524" layer="21" curve="59.743278"/>
<wire x1="1.5142" y1="0.9318" x2="1.778" y2="0" width="0.1524" layer="51" curve="-31.606487"/>
<wire x1="1.5" y1="-0.9546" x2="1.778" y2="0" width="0.1524" layer="51" curve="32.472615"/>
<wire x1="-1.778" y1="0" x2="-1.5142" y2="-0.9318" width="0.1524" layer="51" curve="31.606487"/>
<wire x1="-1.778" y1="0" x2="-1.5" y2="0.9546" width="0.1524" layer="51" curve="-32.472615"/>
<wire x1="-0.635" y1="0" x2="0" y2="0.635" width="0.1524" layer="51" curve="-90"/>
<wire x1="-1.016" y1="0" x2="0" y2="1.016" width="0.1524" layer="51" curve="-90"/>
<wire x1="0" y1="-0.635" x2="0.635" y2="0" width="0.1524" layer="51" curve="90"/>
<wire x1="0.0539" y1="-1.0699" x2="1.0699" y2="-0.0539" width="0.1524" layer="51" curve="90"/>
<circle x="0" y="0" radius="2.667" width="0.1524" layer="21"/>
<circle x="0" y="0" radius="2.413" width="0.254" layer="21"/>
<pad name="K" x="-1.27" y="0" drill="0.8128" shape="long" rot="R90"/>
<pad name="A" x="1.27" y="0" drill="0.8128" shape="long" rot="R90"/>
<text x="-1.27" y="3.048" size="1.27" layer="25" ratio="10">&gt;NAME</text>
<text x="-1.27" y="-4.318" size="1.27" layer="27" ratio="10">&gt;VALUE</text>
</package>
<package name="SFH482">
<description>&lt;B&gt;IR LED&lt;/B&gt;&lt;p&gt;
infrared emitting diode, Infineon
TO-18, lead spacing 2.54 mm, cathode marking&lt;p&gt;
Inifineon</description>
<wire x1="-2.159" y1="1.524" x2="-2.794" y2="2.159" width="0.1524" layer="21"/>
<wire x1="-2.794" y1="2.159" x2="-2.159" y2="2.794" width="0.1524" layer="21"/>
<wire x1="-1.524" y1="2.159" x2="-2.159" y2="2.794" width="0.1524" layer="21"/>
<wire x1="0" y1="1.778" x2="1.5358" y2="0.8959" width="0.1524" layer="21" curve="-59.743278"/>
<wire x1="-1.5358" y1="0.8959" x2="0" y2="1.778" width="0.1524" layer="21" curve="-59.743278"/>
<wire x1="-1.5358" y1="-0.8959" x2="0" y2="-1.778" width="0.1524" layer="21" curve="59.743278"/>
<wire x1="0" y1="-1.778" x2="1.5358" y2="-0.8959" width="0.1524" layer="21" curve="59.743278"/>
<wire x1="1.5142" y1="0.9318" x2="1.778" y2="0" width="0.1524" layer="51" curve="-31.606487"/>
<wire x1="1.5" y1="-0.9546" x2="1.778" y2="0" width="0.1524" layer="51" curve="32.472615"/>
<wire x1="-1.778" y1="0" x2="-1.5142" y2="-0.9318" width="0.1524" layer="51" curve="31.606487"/>
<wire x1="-1.778" y1="0" x2="-1.5" y2="0.9546" width="0.1524" layer="51" curve="-32.472615"/>
<wire x1="-0.635" y1="0" x2="0" y2="0.635" width="0.1524" layer="51" curve="-90"/>
<wire x1="-1.016" y1="0" x2="0" y2="1.016" width="0.1524" layer="51" curve="-90"/>
<wire x1="0" y1="-0.635" x2="0.635" y2="0" width="0.1524" layer="51" curve="90"/>
<wire x1="0.0539" y1="-1.0699" x2="1.0699" y2="-0.0539" width="0.1524" layer="51" curve="90"/>
<circle x="0" y="0" radius="2.667" width="0.1524" layer="21"/>
<circle x="0" y="0" radius="2.413" width="0.254" layer="21"/>
<pad name="K" x="1.27" y="0" drill="0.8128" shape="long" rot="R90"/>
<pad name="A" x="-1.27" y="0" drill="0.8128" shape="long" rot="R90"/>
<text x="-1.27" y="3.048" size="1.27" layer="25" ratio="10">&gt;NAME</text>
<text x="-1.27" y="-4.318" size="1.27" layer="27" ratio="10">&gt;VALUE</text>
</package>
<package name="U57X32">
<description>&lt;B&gt;LED&lt;/B&gt;&lt;p&gt;
rectangle, 5.7 x 3.2 mm</description>
<wire x1="-3.175" y1="1.905" x2="3.175" y2="1.905" width="0.1524" layer="21"/>
<wire x1="3.175" y1="-1.905" x2="3.175" y2="1.905" width="0.1524" layer="21"/>
<wire x1="3.175" y1="-1.905" x2="-3.175" y2="-1.905" width="0.1524" layer="21"/>
<wire x1="-3.175" y1="1.905" x2="-3.175" y2="-1.905" width="0.1524" layer="21"/>
<wire x1="-2.667" y1="1.397" x2="2.667" y2="1.397" width="0.1524" layer="21"/>
<wire x1="2.667" y1="-1.397" x2="2.667" y2="1.397" width="0.1524" layer="21"/>
<wire x1="2.667" y1="-1.397" x2="-2.667" y2="-1.397" width="0.1524" layer="21"/>
<wire x1="-2.667" y1="1.397" x2="-2.667" y2="-1.397" width="0.1524" layer="21"/>
<wire x1="-2.54" y1="1.016" x2="2.54" y2="1.016" width="0.1524" layer="51"/>
<wire x1="2.286" y1="1.27" x2="2.286" y2="-1.27" width="0.1524" layer="51"/>
<wire x1="-2.54" y1="0.508" x2="2.54" y2="0.508" width="0.1524" layer="51"/>
<wire x1="-2.54" y1="0" x2="2.54" y2="0" width="0.1524" layer="51"/>
<wire x1="-2.54" y1="-0.508" x2="2.54" y2="-0.508" width="0.1524" layer="51"/>
<wire x1="-2.54" y1="-1.016" x2="2.54" y2="-1.016" width="0.1524" layer="51"/>
<wire x1="-2.286" y1="1.27" x2="-2.286" y2="-1.27" width="0.1524" layer="51"/>
<wire x1="-1.778" y1="1.27" x2="-1.778" y2="-1.27" width="0.1524" layer="51"/>
<wire x1="-1.27" y1="1.27" x2="-1.27" y2="-1.27" width="0.1524" layer="51"/>
<wire x1="-0.762" y1="1.27" x2="-0.762" y2="-1.27" width="0.1524" layer="51"/>
<wire x1="-0.254" y1="1.27" x2="-0.254" y2="-1.27" width="0.1524" layer="51"/>
<wire x1="0.254" y1="1.27" x2="0.254" y2="-1.27" width="0.1524" layer="51"/>
<wire x1="0.762" y1="1.27" x2="0.762" y2="-1.27" width="0.1524" layer="51"/>
<wire x1="1.27" y1="1.27" x2="1.27" y2="-1.27" width="0.1524" layer="51"/>
<wire x1="1.778" y1="1.27" x2="1.778" y2="-1.27" width="0.1524" layer="51"/>
<pad name="A" x="-1.27" y="0" drill="0.8128" shape="long" rot="R90"/>
<pad name="K" x="1.27" y="0" drill="0.8128" shape="long" rot="R90"/>
<text x="3.683" y="0.254" size="1.27" layer="25" ratio="10">&gt;NAME</text>
<text x="3.683" y="-1.524" size="1.27" layer="27" ratio="10">&gt;VALUE</text>
</package>
<package name="IRL80A">
<description>&lt;B&gt;IR LED&lt;/B&gt;&lt;p&gt;
IR transmitter Siemens</description>
<wire x1="0.889" y1="2.286" x2="0.889" y2="1.778" width="0.1524" layer="21"/>
<wire x1="0.889" y1="1.778" x2="0.889" y2="0.762" width="0.1524" layer="51"/>
<wire x1="0.889" y1="0.762" x2="0.889" y2="-0.635" width="0.1524" layer="21"/>
<wire x1="0.889" y1="-0.635" x2="0.889" y2="-1.778" width="0.1524" layer="51"/>
<wire x1="0.889" y1="-1.778" x2="0.889" y2="-2.286" width="0.1524" layer="21"/>
<wire x1="0.889" y1="-2.286" x2="-0.889" y2="-2.286" width="0.1524" layer="21"/>
<wire x1="-0.889" y1="2.286" x2="-0.889" y2="1.778" width="0.1524" layer="21"/>
<wire x1="-0.889" y1="1.778" x2="-0.889" y2="0.762" width="0.1524" layer="51"/>
<wire x1="-0.889" y1="0.762" x2="-0.889" y2="-0.762" width="0.1524" layer="21"/>
<wire x1="-0.889" y1="-0.762" x2="-0.889" y2="-1.778" width="0.1524" layer="51"/>
<wire x1="-0.889" y1="-1.778" x2="-0.889" y2="-2.286" width="0.1524" layer="21"/>
<wire x1="-0.889" y1="2.286" x2="0.889" y2="2.286" width="0.1524" layer="21"/>
<wire x1="-0.889" y1="-0.762" x2="-0.889" y2="0.762" width="0.1524" layer="21" curve="-180"/>
<wire x1="-1.397" y1="0.254" x2="-1.397" y2="-0.254" width="0.0508" layer="21"/>
<wire x1="-1.143" y1="0.508" x2="-1.143" y2="-0.508" width="0.0508" layer="21"/>
<pad name="K" x="0" y="1.27" drill="0.8128" shape="octagon"/>
<pad name="A" x="0" y="-1.27" drill="0.8128" shape="octagon"/>
<text x="1.27" y="0.381" size="1.27" layer="25" ratio="10">&gt;NAME</text>
<text x="1.27" y="-1.651" size="1.27" layer="27" ratio="10">&gt;VALUE</text>
</package>
<package name="P-LCC-2">
<description>&lt;b&gt;TOPLED® High-optical Power LED (HOP)&lt;/b&gt;&lt;p&gt;
Source: http://www.osram.convergy.de/ ... ls_t675.pdf</description>
<wire x1="-1.4" y1="-1.05" x2="-1.4" y2="-1.6" width="0.2032" layer="51"/>
<wire x1="-1.4" y1="-1.6" x2="-1.1" y2="-1.6" width="0.2032" layer="51"/>
<wire x1="-1.1" y1="-1.6" x2="-0.85" y2="-1.6" width="0.2032" layer="51"/>
<wire x1="-0.85" y1="-1.6" x2="1.1" y2="-1.6" width="0.2032" layer="51"/>
<wire x1="1.1" y1="-1.6" x2="1.4" y2="-1.6" width="0.2032" layer="51"/>
<wire x1="1.4" y1="-1.6" x2="1.4" y2="1.6" width="0.2032" layer="51"/>
<wire x1="1.4" y1="1.6" x2="1.1" y2="1.6" width="0.2032" layer="51"/>
<wire x1="1.1" y1="1.6" x2="-1.1" y2="1.6" width="0.2032" layer="51"/>
<wire x1="-1.1" y1="1.6" x2="-1.4" y2="1.6" width="0.2032" layer="51"/>
<wire x1="-1.1" y1="1.6" x2="-1.1" y2="1.8" width="0.1016" layer="51"/>
<wire x1="-1.1" y1="1.8" x2="1.1" y2="1.8" width="0.1016" layer="51"/>
<wire x1="1.1" y1="1.8" x2="1.1" y2="1.6" width="0.1016" layer="51"/>
<wire x1="-1.1" y1="-1.6" x2="-1.1" y2="-1.8" width="0.1016" layer="51"/>
<wire x1="-1.1" y1="-1.8" x2="1.1" y2="-1.8" width="0.1016" layer="51"/>
<wire x1="1.1" y1="-1.8" x2="1.1" y2="-1.6" width="0.1016" layer="51"/>
<wire x1="-0.85" y1="-1.6" x2="-1.4" y2="-1.05" width="0.2032" layer="51"/>
<wire x1="-1.4" y1="1.6" x2="-1.4" y2="-1.05" width="0.2032" layer="51"/>
<circle x="0" y="0" radius="1.1" width="0.2032" layer="51"/>
<smd name="C" x="0" y="-2.75" dx="4" dy="4" layer="1" stop="no" cream="no"/>
<smd name="A" x="0" y="2.75" dx="4" dy="4" layer="1" stop="no" cream="no"/>
<text x="-2.54" y="-1.905" size="1.27" layer="25" rot="R90">&gt;NAME</text>
<text x="3.81" y="-1.905" size="1.27" layer="27" rot="R90">&gt;VALUE</text>
<text x="-0.635" y="2.54" size="1.27" layer="21">A</text>
<text x="-0.635" y="-3.81" size="1.27" layer="21">C</text>
<rectangle x1="-1.3" y1="-2.25" x2="1.3" y2="-0.75" layer="31"/>
<rectangle x1="-1.3" y1="0.75" x2="1.3" y2="2.25" layer="31"/>
<rectangle x1="-0.25" y1="-0.25" x2="0.25" y2="0.25" layer="21"/>
<rectangle x1="-1.4" y1="0.65" x2="1.4" y2="2.35" layer="29"/>
<rectangle x1="-1.4" y1="-2.35" x2="1.4" y2="-0.65" layer="29"/>
</package>
<package name="OSRAM-MINI-TOP-LED">
<description>&lt;b&gt;BLUE LINETM Hyper Mini TOPLED® Hyper-Bright LED&lt;/b&gt;&lt;p&gt;
Source: http://www.osram.convergy.de/ ... LB M676.pdf</description>
<wire x1="-0.6" y1="0.9" x2="-0.6" y2="-0.7" width="0.1016" layer="51"/>
<wire x1="-0.45" y1="-0.9" x2="-0.4" y2="-0.9" width="0.1016" layer="51"/>
<wire x1="-0.4" y1="-0.9" x2="0.6" y2="-0.9" width="0.1016" layer="51"/>
<wire x1="0.6" y1="-0.9" x2="0.6" y2="0.9" width="0.1016" layer="51"/>
<wire x1="0.6" y1="0.9" x2="-0.6" y2="0.9" width="0.1016" layer="51"/>
<wire x1="-0.45" y1="0.95" x2="-0.45" y2="1.1" width="0.1016" layer="51"/>
<wire x1="-0.45" y1="1.1" x2="0.45" y2="1.1" width="0.1016" layer="51"/>
<wire x1="0.45" y1="1.1" x2="0.45" y2="0.95" width="0.1016" layer="51"/>
<wire x1="-0.6" y1="-0.7" x2="-0.4" y2="-0.9" width="0.1016" layer="51"/>
<wire x1="-0.45" y1="-0.9" x2="-0.45" y2="-1.1" width="0.1016" layer="51"/>
<wire x1="-0.45" y1="-1.1" x2="0.45" y2="-1.1" width="0.1016" layer="51"/>
<wire x1="0.45" y1="-1.1" x2="0.45" y2="-0.95" width="0.1016" layer="51"/>
<smd name="A" x="0" y="2.6" dx="4" dy="4" layer="1" stop="no" cream="no"/>
<smd name="C" x="0" y="-2.6" dx="4" dy="4" layer="1" stop="no" cream="no"/>
<text x="-0.635" y="1.905" size="1.27" layer="21">A</text>
<text x="-0.635" y="-3.175" size="1.27" layer="21">C</text>
<text x="-2.54" y="-1.905" size="1.27" layer="25" rot="R90">&gt;NAME</text>
<text x="3.81" y="-1.905" size="1.27" layer="27" rot="R90">&gt;VALUE</text>
<rectangle x1="-0.5" y1="0.6" x2="0.5" y2="1.4" layer="29"/>
<rectangle x1="-0.5" y1="-1.4" x2="0.5" y2="-0.6" layer="29"/>
<rectangle x1="-0.15" y1="-0.6" x2="0.15" y2="-0.3" layer="51"/>
<rectangle x1="-0.45" y1="0.65" x2="0.45" y2="1.35" layer="31"/>
<rectangle x1="-0.45" y1="-1.35" x2="0.45" y2="-0.65" layer="31"/>
</package>
<package name="OSRAM-SIDELED">
<description>&lt;b&gt;Super SIDELED® High-Current LED&lt;/b&gt;&lt;p&gt;
LG A672, LP A672 &lt;br&gt;
Source: http://www.osram.convergy.de/ ... LG_LP_A672.pdf (2004.05.13)</description>
<wire x1="-1.85" y1="-2.05" x2="-1.85" y2="-0.75" width="0.1016" layer="51"/>
<wire x1="-1.85" y1="-0.75" x2="-1.7" y2="-0.75" width="0.1016" layer="51"/>
<wire x1="-1.7" y1="-0.75" x2="-1.7" y2="0.75" width="0.1016" layer="51"/>
<wire x1="-1.7" y1="0.75" x2="-1.85" y2="0.75" width="0.1016" layer="51"/>
<wire x1="-1.85" y1="0.75" x2="-1.85" y2="2.05" width="0.1016" layer="51"/>
<wire x1="-1.85" y1="2.05" x2="0.9" y2="2.05" width="0.1016" layer="51"/>
<wire x1="0.9" y1="2.05" x2="0.9" y2="-2.05" width="0.1016" layer="51"/>
<wire x1="0.9" y1="-2.05" x2="-1.85" y2="-2.05" width="0.1016" layer="51"/>
<wire x1="0.9" y1="-2.05" x2="1.05" y2="-2.05" width="0.1016" layer="51"/>
<wire x1="1.05" y1="-2.05" x2="1.85" y2="-1.85" width="0.1016" layer="51"/>
<wire x1="1.85" y1="-1.85" x2="1.85" y2="1.85" width="0.1016" layer="51"/>
<wire x1="1.85" y1="1.85" x2="1.05" y2="2.05" width="0.1016" layer="51"/>
<wire x1="1.05" y1="2.05" x2="0.9" y2="2.05" width="0.1016" layer="51"/>
<wire x1="1.05" y1="2.05" x2="1.05" y2="-2.05" width="0.1016" layer="51"/>
<wire x1="-0.55" y1="-0.9" x2="-0.55" y2="0.9" width="0.1016" layer="51" curve="-167.319617"/>
<wire x1="-0.55" y1="-0.9" x2="0.85" y2="-1.2" width="0.1016" layer="51" style="shortdash"/>
<wire x1="-0.55" y1="0.9" x2="0.85" y2="1.2" width="0.1016" layer="51" style="shortdash"/>
<smd name="C" x="0" y="-2.5" dx="4" dy="4" layer="1" stop="no" cream="no"/>
<smd name="A" x="0" y="2.5" dx="4" dy="4" layer="1" stop="no" cream="no"/>
<text x="0.635" y="-3.175" size="1.27" layer="21" rot="R90">C</text>
<text x="0.635" y="2.54" size="1.27" layer="21" rot="R90">A</text>
<text x="-2.54" y="-2.54" size="1.27" layer="25" rot="R90">&gt;NAME</text>
<text x="3.81" y="-2.54" size="1.27" layer="27" rot="R90">&gt;VALUE</text>
<rectangle x1="-2.1" y1="-2.2" x2="2.1" y2="-0.4" layer="29"/>
<rectangle x1="-2.1" y1="0.4" x2="2.1" y2="2.2" layer="29"/>
<rectangle x1="-1.9" y1="-2.1" x2="1.9" y2="-0.6" layer="31"/>
<rectangle x1="-1.9" y1="0.6" x2="1.9" y2="2.1" layer="31"/>
<rectangle x1="-1.85" y1="-2.05" x2="-0.7" y2="-1" layer="51"/>
</package>
<package name="SMART-LED">
<description>&lt;b&gt;SmartLEDTM Hyper-Bright LED&lt;/b&gt;&lt;p&gt;
Source: http://www.osram.convergy.de/ ... LA_LO_LS_LY L896.pdf</description>
<wire x1="-0.35" y1="0.6" x2="0.35" y2="0.6" width="0.1016" layer="51" style="shortdash"/>
<wire x1="0.35" y1="0.6" x2="0.35" y2="-0.6" width="0.1016" layer="21" style="shortdash"/>
<wire x1="0.35" y1="-0.6" x2="0.15" y2="-0.6" width="0.1016" layer="51" style="shortdash"/>
<wire x1="0.15" y1="-0.6" x2="-0.35" y2="-0.6" width="0.1016" layer="51" style="shortdash"/>
<wire x1="-0.35" y1="-0.6" x2="-0.35" y2="0.6" width="0.1016" layer="21" style="shortdash"/>
<wire x1="0.35" y1="-0.4" x2="0.15" y2="-0.6" width="0.1016" layer="51" style="shortdash"/>
<smd name="A" x="0" y="0.725" dx="0.35" dy="0.35" layer="1"/>
<smd name="B" x="0" y="-0.725" dx="0.35" dy="0.35" layer="1"/>
<text x="-0.635" y="-0.635" size="1.016" layer="25" rot="R90">&gt;NAME</text>
<text x="1.905" y="-0.635" size="1.016" layer="27" rot="R90">&gt;VALUE</text>
<rectangle x1="-0.15" y1="-0.35" x2="0.15" y2="-0.05" layer="21"/>
<rectangle x1="-0.15" y1="0.6" x2="0.15" y2="0.85" layer="51"/>
<rectangle x1="-0.15" y1="-0.85" x2="0.15" y2="-0.6" layer="51"/>
</package>
<package name="P-LCC-2-TOPLED-RG">
<description>&lt;b&gt;Hyper TOPLED® RG Hyper-Bright LED&lt;/b&gt;&lt;p&gt;
Source: http://www.osram.convergy.de/ ... LA_LO_LS_LY T776.pdf</description>
<wire x1="-1.4" y1="-1.05" x2="-1.4" y2="-1.6" width="0.2032" layer="51"/>
<wire x1="-1.4" y1="-1.6" x2="-1.1" y2="-1.6" width="0.2032" layer="51"/>
<wire x1="-1.1" y1="-1.6" x2="-0.85" y2="-1.6" width="0.2032" layer="51"/>
<wire x1="-0.85" y1="-1.6" x2="1.1" y2="-1.6" width="0.2032" layer="51"/>
<wire x1="1.1" y1="-1.6" x2="1.4" y2="-1.6" width="0.2032" layer="51"/>
<wire x1="1.4" y1="-1.6" x2="1.4" y2="1.6" width="0.2032" layer="51"/>
<wire x1="1.4" y1="1.6" x2="1.1" y2="1.6" width="0.2032" layer="51"/>
<wire x1="1.1" y1="1.6" x2="-1.1" y2="1.6" width="0.2032" layer="51"/>
<wire x1="-1.1" y1="1.6" x2="-1.4" y2="1.6" width="0.2032" layer="51"/>
<wire x1="-1.1" y1="1.6" x2="-1.1" y2="2.45" width="0.1016" layer="51"/>
<wire x1="1.1" y1="2.45" x2="1.1" y2="1.6" width="0.1016" layer="51"/>
<wire x1="-1.1" y1="-1.6" x2="-1.1" y2="-2.45" width="0.1016" layer="51"/>
<wire x1="1.1" y1="-2.45" x2="1.1" y2="-1.6" width="0.1016" layer="51"/>
<wire x1="-0.85" y1="-1.6" x2="-1.4" y2="-1.05" width="0.2032" layer="51"/>
<wire x1="-1.4" y1="1.6" x2="-1.4" y2="-1.05" width="0.2032" layer="51"/>
<circle x="0" y="0" radius="1.1" width="0.2032" layer="21"/>
<smd name="C" x="0" y="-3.5" dx="4" dy="4" layer="1" stop="no" cream="no"/>
<smd name="A" x="0" y="3.5" dx="4" dy="4" layer="1" stop="no" cream="no"/>
<text x="-2.54" y="-1.905" size="1.27" layer="25" rot="R90">&gt;NAME</text>
<text x="3.81" y="-1.905" size="1.27" layer="27" rot="R90">&gt;VALUE</text>
<text x="-0.635" y="3.29" size="1.27" layer="21">A</text>
<text x="-0.635" y="-4.56" size="1.27" layer="21">C</text>
<rectangle x1="-1.3" y1="-3" x2="1.3" y2="-1.5" layer="31"/>
<rectangle x1="-1.3" y1="1.5" x2="1.3" y2="3" layer="31"/>
<rectangle x1="-0.25" y1="-0.25" x2="0.25" y2="0.25" layer="21"/>
<rectangle x1="-1.15" y1="2.4" x2="1.15" y2="2.7" layer="51"/>
<rectangle x1="-1.15" y1="-2.7" x2="1.15" y2="-2.4" layer="51"/>
<rectangle x1="-1.5" y1="1.5" x2="1.5" y2="3.2" layer="29"/>
<rectangle x1="-1.5" y1="-3.2" x2="1.5" y2="-1.5" layer="29"/>
<hole x="0" y="0" drill="2.8"/>
</package>
<package name="MICRO-SIDELED">
<description>&lt;b&gt;Hyper Micro SIDELED®&lt;/b&gt;&lt;p&gt;
Source: http://www.osram.convergy.de/ ... LA_LO_LS_LY Y876.pdf</description>
<wire x1="0.65" y1="1.1" x2="-0.1" y2="1.1" width="0.1016" layer="51"/>
<wire x1="-0.1" y1="1.1" x2="-0.35" y2="1" width="0.1016" layer="51"/>
<wire x1="-0.35" y1="1" x2="-0.35" y2="-0.9" width="0.1016" layer="21"/>
<wire x1="-0.35" y1="-0.9" x2="-0.1" y2="-1.1" width="0.1016" layer="51"/>
<wire x1="-0.1" y1="-1.1" x2="0.65" y2="-1.1" width="0.1016" layer="51"/>
<wire x1="0.65" y1="-1.1" x2="0.65" y2="1.1" width="0.1016" layer="21"/>
<wire x1="0.6" y1="0.9" x2="0.25" y2="0.7" width="0.0508" layer="21"/>
<wire x1="0.25" y1="0.7" x2="0.25" y2="-0.7" width="0.0508" layer="21"/>
<wire x1="0.25" y1="-0.7" x2="0.6" y2="-0.9" width="0.0508" layer="21"/>
<smd name="A" x="0" y="1.95" dx="1.6" dy="1.6" layer="1" stop="no" cream="no"/>
<smd name="C" x="0" y="-1.95" dx="1.6" dy="1.6" layer="1" stop="no" cream="no"/>
<text x="-1.27" y="-1.905" size="1.27" layer="25" rot="R90">&gt;NAME</text>
<text x="2.54" y="-1.905" size="1.27" layer="27" rot="R90">&gt;VALUE</text>
<rectangle x1="-0.4" y1="1.1" x2="0.4" y2="1.8" layer="29"/>
<rectangle x1="-0.4" y1="-1.8" x2="0.4" y2="-1.1" layer="29"/>
<rectangle x1="-0.35" y1="-1.75" x2="0.35" y2="-1.15" layer="31"/>
<rectangle x1="-0.35" y1="1.15" x2="0.35" y2="1.75" layer="31"/>
<rectangle x1="-0.125" y1="1.125" x2="0.125" y2="1.75" layer="51"/>
<rectangle x1="-0.125" y1="-1.75" x2="0.125" y2="-1.125" layer="51"/>
</package>
<package name="P-LCC-4">
<description>&lt;b&gt;Power TOPLED®&lt;/b&gt;&lt;p&gt;
Source: http://www.osram.convergy.de/ ... LA_LO_LA_LY E67B.pdf</description>
<wire x1="-1.4" y1="-1.05" x2="-1.4" y2="-1.6" width="0.2032" layer="51"/>
<wire x1="-1.4" y1="-1.6" x2="-1" y2="-1.6" width="0.2032" layer="51"/>
<wire x1="-1" y1="-1.6" x2="-0.85" y2="-1.6" width="0.2032" layer="51"/>
<wire x1="-0.85" y1="-1.6" x2="1" y2="-1.6" width="0.2032" layer="51"/>
<wire x1="1" y1="-1.6" x2="1.4" y2="-1.6" width="0.2032" layer="51"/>
<wire x1="1.4" y1="-1.6" x2="1.4" y2="1.6" width="0.2032" layer="51"/>
<wire x1="1.4" y1="1.6" x2="1.1" y2="1.6" width="0.2032" layer="51"/>
<wire x1="1.1" y1="1.6" x2="-1" y2="1.6" width="0.2032" layer="51"/>
<wire x1="-1" y1="1.6" x2="-1.4" y2="1.6" width="0.2032" layer="51"/>
<wire x1="-1" y1="1.6" x2="-1" y2="1.8" width="0.1016" layer="51"/>
<wire x1="-1" y1="1.8" x2="-0.5" y2="1.8" width="0.1016" layer="51"/>
<wire x1="-0.5" y1="1.8" x2="-0.5" y2="1.65" width="0.1016" layer="51"/>
<wire x1="0.5" y1="1.65" x2="0.5" y2="1.8" width="0.1016" layer="51"/>
<wire x1="0.5" y1="1.8" x2="1.1" y2="1.8" width="0.1016" layer="51"/>
<wire x1="1.1" y1="1.8" x2="1.1" y2="1.6" width="0.1016" layer="51"/>
<wire x1="-1" y1="-1.6" x2="-1" y2="-1.8" width="0.1016" layer="51"/>
<wire x1="-1" y1="-1.8" x2="-0.5" y2="-1.8" width="0.1016" layer="51"/>
<wire x1="-0.5" y1="-1.8" x2="-0.5" y2="-1.65" width="0.1016" layer="51"/>
<wire x1="0.5" y1="-1.65" x2="0.5" y2="-1.8" width="0.1016" layer="51"/>
<wire x1="0.5" y1="-1.8" x2="1" y2="-1.8" width="0.1016" layer="51"/>
<wire x1="1" y1="-1.8" x2="1" y2="-1.6" width="0.1016" layer="51"/>
<wire x1="-0.85" y1="-1.6" x2="-1.4" y2="-1.05" width="0.2032" layer="51"/>
<wire x1="-1.4" y1="1.6" x2="-1.4" y2="-1.05" width="0.2032" layer="51"/>
<circle x="0" y="0" radius="1.1" width="0.2032" layer="51"/>
<smd name="A" x="-2" y="3.15" dx="3.3" dy="4.8" layer="1" stop="no" cream="no"/>
<smd name="C@3" x="2" y="3.15" dx="3.3" dy="4.8" layer="1" stop="no" cream="no"/>
<smd name="C@4" x="2" y="-3.15" dx="3.3" dy="4.8" layer="1" stop="no" cream="no"/>
<smd name="C@1" x="-2" y="-3.15" dx="3.3" dy="4.8" layer="1" stop="no" cream="no"/>
<text x="-3.81" y="-2.54" size="1.27" layer="25" rot="R90">&gt;NAME</text>
<text x="5.08" y="-2.54" size="1.27" layer="27" rot="R90">&gt;VALUE</text>
<text x="-1.905" y="-3.81" size="1.27" layer="21">C</text>
<text x="-1.905" y="2.54" size="1.27" layer="21">A</text>
<text x="1.27" y="2.54" size="1.27" layer="21">C</text>
<text x="1.27" y="-3.81" size="1.27" layer="21">C</text>
<rectangle x1="-1.15" y1="0.75" x2="-0.35" y2="1.85" layer="29"/>
<rectangle x1="0.35" y1="0.75" x2="1.15" y2="1.85" layer="29"/>
<rectangle x1="0.35" y1="-1.85" x2="1.15" y2="-0.75" layer="29"/>
<rectangle x1="-1.15" y1="-1.85" x2="-0.35" y2="-0.75" layer="29"/>
<rectangle x1="-1.1" y1="-1.8" x2="-0.4" y2="-0.8" layer="31"/>
<rectangle x1="0.4" y1="-1.8" x2="1.1" y2="-0.8" layer="31"/>
<rectangle x1="0.4" y1="0.8" x2="1.1" y2="1.8" layer="31"/>
<rectangle x1="-1.1" y1="0.8" x2="-0.4" y2="1.8" layer="31"/>
<rectangle x1="-0.2" y1="-0.2" x2="0.2" y2="0.2" layer="21"/>
</package>
<package name="CHIP-LED0603">
<description>&lt;b&gt;Hyper CHIPLED Hyper-Bright LED&lt;/b&gt;&lt;p&gt;
LB Q993&lt;br&gt;
Source: http://www.osram.convergy.de/ ... Lb_q993.pdf</description>
<wire x1="-0.4" y1="0.45" x2="-0.4" y2="-0.45" width="0.1016" layer="51"/>
<wire x1="0.4" y1="0.45" x2="0.4" y2="-0.45" width="0.1016" layer="51"/>
<smd name="C" x="0" y="0.75" dx="0.8" dy="0.8" layer="1"/>
<smd name="A" x="0" y="-0.75" dx="0.8" dy="0.8" layer="1"/>
<text x="-0.635" y="-0.635" size="1.27" layer="25" rot="R90">&gt;NAME</text>
<text x="1.905" y="-0.635" size="1.27" layer="27" rot="R90">&gt;VALUE</text>
<rectangle x1="-0.45" y1="0.45" x2="0.45" y2="0.85" layer="51"/>
<rectangle x1="-0.45" y1="-0.85" x2="0.45" y2="-0.45" layer="51"/>
<rectangle x1="-0.45" y1="0" x2="-0.3" y2="0.3" layer="21"/>
<rectangle x1="0.3" y1="0" x2="0.45" y2="0.3" layer="21"/>
<rectangle x1="-0.15" y1="0" x2="0.15" y2="0.3" layer="21"/>
</package>
<package name="CHIP-LED0805">
<description>&lt;b&gt;Hyper CHIPLED Hyper-Bright LED&lt;/b&gt;&lt;p&gt;
LB R99A&lt;br&gt;
Source: http://www.osram.convergy.de/ ... lb_r99a.pdf</description>
<wire x1="-0.625" y1="0.45" x2="-0.625" y2="-0.45" width="0.1016" layer="51"/>
<wire x1="0.625" y1="0.45" x2="0.625" y2="-0.475" width="0.1016" layer="51"/>
<smd name="C" x="0" y="1.05" dx="1.2" dy="1.2" layer="1"/>
<smd name="A" x="0" y="-1.05" dx="1.2" dy="1.2" layer="1"/>
<text x="-1.27" y="-1.27" size="1.27" layer="25" rot="R90">&gt;NAME</text>
<text x="2.54" y="-1.27" size="1.27" layer="27" rot="R90">&gt;VALUE</text>
<rectangle x1="-0.675" y1="0" x2="-0.525" y2="0.3" layer="21"/>
<rectangle x1="0.525" y1="0" x2="0.675" y2="0.3" layer="21"/>
<rectangle x1="-0.15" y1="0" x2="0.15" y2="0.3" layer="21"/>
<rectangle x1="-0.675" y1="0.45" x2="0.675" y2="1.05" layer="51"/>
<rectangle x1="-0.675" y1="-1.05" x2="0.675" y2="-0.45" layer="51"/>
</package>
<package name="MINI-TOPLED-SANTANA">
<description>&lt;b&gt;Mini TOPLED Santana®&lt;/b&gt;&lt;p&gt;
Source: http://www.osram.convergy.de/ ... LG M470.pdf</description>
<wire x1="0.7" y1="-1" x2="0.35" y2="-1" width="0.1016" layer="21"/>
<wire x1="0.35" y1="-1" x2="-0.7" y2="-1" width="0.1016" layer="21"/>
<wire x1="-0.7" y1="-1" x2="-0.7" y2="1" width="0.1016" layer="21"/>
<wire x1="-0.7" y1="1" x2="0.7" y2="1" width="0.1016" layer="21"/>
<wire x1="0.7" y1="1" x2="0.7" y2="-0.65" width="0.1016" layer="21"/>
<wire x1="0.7" y1="-0.65" x2="0.7" y2="-1" width="0.1016" layer="21"/>
<wire x1="0.45" y1="-0.7" x2="-0.45" y2="-0.7" width="0.1016" layer="21"/>
<wire x1="-0.45" y1="-0.7" x2="-0.45" y2="0.7" width="0.1016" layer="21"/>
<wire x1="-0.45" y1="0.7" x2="0.45" y2="0.7" width="0.1016" layer="21"/>
<wire x1="0.45" y1="0.7" x2="0.45" y2="-0.7" width="0.1016" layer="21"/>
<wire x1="0.7" y1="-0.65" x2="0.35" y2="-1" width="0.1016" layer="21"/>
<smd name="C" x="0" y="-2.2" dx="1.6" dy="1.6" layer="1" stop="no" cream="no"/>
<smd name="A" x="0" y="2.2" dx="1.6" dy="1.6" layer="1" stop="no" cream="no"/>
<text x="-1.27" y="-1.905" size="1.27" layer="25" rot="R90">&gt;NAME</text>
<text x="2.54" y="-1.905" size="1.27" layer="27" rot="R90">&gt;VALUE</text>
<rectangle x1="-0.55" y1="1.5" x2="0.55" y2="2.1" layer="29"/>
<rectangle x1="-0.55" y1="-2.1" x2="0.55" y2="-1.5" layer="29"/>
<rectangle x1="-0.5" y1="-2.05" x2="0.5" y2="-1.55" layer="31"/>
<rectangle x1="-0.5" y1="1.55" x2="0.5" y2="2.05" layer="31"/>
<rectangle x1="-0.2" y1="-0.4" x2="0.15" y2="-0.05" layer="21"/>
<rectangle x1="-0.5" y1="-2.1" x2="0.5" y2="-1.4" layer="51"/>
<rectangle x1="-0.5" y1="1.4" x2="0.5" y2="2.05" layer="51"/>
<rectangle x1="-0.5" y1="1" x2="0.5" y2="1.4" layer="21"/>
<rectangle x1="-0.5" y1="-1.4" x2="0.5" y2="-1.05" layer="21"/>
<hole x="0" y="0" drill="2.7"/>
</package>
<package name="CHIPLED_0805">
<description>&lt;b&gt;CHIPLED&lt;/b&gt;&lt;p&gt;
Source: http://www.osram.convergy.de/ ... LG_R971.pdf</description>
<wire x1="-0.35" y1="0.925" x2="0.35" y2="0.925" width="0.1016" layer="51" curve="162.394521"/>
<wire x1="-0.35" y1="-0.925" x2="0.35" y2="-0.925" width="0.1016" layer="51" curve="-162.394521"/>
<wire x1="0.575" y1="0.525" x2="0.575" y2="-0.525" width="0.1016" layer="51"/>
<wire x1="-0.575" y1="-0.5" x2="-0.575" y2="0.925" width="0.1016" layer="51"/>
<circle x="-0.45" y="0.85" radius="0.103" width="0.1016" layer="51"/>
<smd name="C" x="0" y="1.05" dx="1.2" dy="1.2" layer="1"/>
<smd name="A" x="0" y="-1.05" dx="1.2" dy="1.2" layer="1"/>
<text x="-1.27" y="-1.27" size="1.27" layer="25" rot="R90">&gt;NAME</text>
<text x="2.54" y="-1.27" size="1.27" layer="27" rot="R90">&gt;VALUE</text>
<rectangle x1="0.3" y1="0.5" x2="0.625" y2="1" layer="51"/>
<rectangle x1="-0.325" y1="0.5" x2="-0.175" y2="0.75" layer="51"/>
<rectangle x1="0.175" y1="0.5" x2="0.325" y2="0.75" layer="51"/>
<rectangle x1="-0.2" y1="0.5" x2="0.2" y2="0.675" layer="51"/>
<rectangle x1="0.3" y1="-1" x2="0.625" y2="-0.5" layer="51"/>
<rectangle x1="-0.625" y1="-1" x2="-0.3" y2="-0.5" layer="51"/>
<rectangle x1="0.175" y1="-0.75" x2="0.325" y2="-0.5" layer="51"/>
<rectangle x1="-0.325" y1="-0.75" x2="-0.175" y2="-0.5" layer="51"/>
<rectangle x1="-0.2" y1="-0.675" x2="0.2" y2="-0.5" layer="51"/>
<rectangle x1="-0.1" y1="0" x2="0.1" y2="0.2" layer="21"/>
<rectangle x1="-0.6" y1="0.5" x2="-0.3" y2="0.8" layer="51"/>
<rectangle x1="-0.625" y1="0.925" x2="-0.3" y2="1" layer="51"/>
</package>
<package name="CHIPLED_1206">
<description>&lt;b&gt;CHIPLED&lt;/b&gt;&lt;p&gt;
Source: http://www.osram.convergy.de/ ... LG_LY N971.pdf</description>
<wire x1="-0.4" y1="1.6" x2="0.4" y2="1.6" width="0.1016" layer="51" curve="172.619069"/>
<wire x1="-0.8" y1="-0.95" x2="-0.8" y2="0.95" width="0.1016" layer="51"/>
<wire x1="0.8" y1="0.95" x2="0.8" y2="-0.95" width="0.1016" layer="51"/>
<circle x="-0.55" y="1.425" radius="0.1" width="0.1016" layer="51"/>
<smd name="C" x="0" y="1.75" dx="1.5" dy="1.5" layer="1"/>
<smd name="A" x="0" y="-1.75" dx="1.5" dy="1.5" layer="1"/>
<text x="-1.27" y="-1.27" size="1.27" layer="25" rot="R90">&gt;NAME</text>
<text x="2.54" y="-1.27" size="1.27" layer="27" rot="R90">&gt;VALUE</text>
<rectangle x1="-0.85" y1="1.525" x2="-0.35" y2="1.65" layer="51"/>
<rectangle x1="-0.85" y1="1.225" x2="-0.625" y2="1.55" layer="51"/>
<rectangle x1="-0.45" y1="1.225" x2="-0.325" y2="1.45" layer="51"/>
<rectangle x1="-0.65" y1="1.225" x2="-0.225" y2="1.35" layer="51"/>
<rectangle x1="0.35" y1="1.3" x2="0.85" y2="1.65" layer="51"/>
<rectangle x1="0.25" y1="1.225" x2="0.85" y2="1.35" layer="51"/>
<rectangle x1="-0.85" y1="0.95" x2="0.85" y2="1.25" layer="51"/>
<rectangle x1="-0.85" y1="-1.65" x2="0.85" y2="-0.95" layer="51"/>
<rectangle x1="-0.85" y1="0.35" x2="-0.525" y2="0.775" layer="21"/>
<rectangle x1="0.525" y1="0.35" x2="0.85" y2="0.775" layer="21"/>
<rectangle x1="-0.175" y1="0" x2="0.175" y2="0.35" layer="21"/>
</package>
<package name="CHIPLED_0603">
<description>&lt;b&gt;CHIPLED&lt;/b&gt;&lt;p&gt;
Source: http://www.osram.convergy.de/ ... LG_LY Q971.pdf</description>
<wire x1="-0.3" y1="0.8" x2="0.3" y2="0.8" width="0.1016" layer="51" curve="170.055574"/>
<wire x1="-0.275" y1="-0.825" x2="0.275" y2="-0.825" width="0.0508" layer="51" curve="-180"/>
<wire x1="-0.4" y1="0.375" x2="-0.4" y2="-0.35" width="0.1016" layer="51"/>
<wire x1="0.4" y1="0.35" x2="0.4" y2="-0.35" width="0.1016" layer="51"/>
<circle x="-0.35" y="0.625" radius="0.075" width="0.0508" layer="51"/>
<smd name="C" x="0" y="0.75" dx="0.8" dy="0.8" layer="1"/>
<smd name="A" x="0" y="-0.75" dx="0.8" dy="0.8" layer="1"/>
<text x="-0.635" y="-1.27" size="1.27" layer="25" rot="R90">&gt;NAME</text>
<text x="1.905" y="-1.27" size="1.27" layer="27" rot="R90">&gt;VALUE</text>
<rectangle x1="-0.45" y1="0.7" x2="-0.25" y2="0.85" layer="51"/>
<rectangle x1="-0.275" y1="0.55" x2="-0.225" y2="0.6" layer="51"/>
<rectangle x1="-0.45" y1="0.35" x2="-0.4" y2="0.725" layer="51"/>
<rectangle x1="0.25" y1="0.55" x2="0.45" y2="0.85" layer="51"/>
<rectangle x1="-0.45" y1="0.35" x2="0.45" y2="0.575" layer="51"/>
<rectangle x1="-0.45" y1="-0.85" x2="-0.25" y2="-0.35" layer="51"/>
<rectangle x1="0.25" y1="-0.85" x2="0.45" y2="-0.35" layer="51"/>
<rectangle x1="-0.275" y1="-0.575" x2="0.275" y2="-0.35" layer="51"/>
<rectangle x1="-0.275" y1="-0.65" x2="-0.175" y2="-0.55" layer="51"/>
<rectangle x1="0.175" y1="-0.65" x2="0.275" y2="-0.55" layer="51"/>
<rectangle x1="-0.125" y1="0" x2="0.125" y2="0.25" layer="21"/>
</package>
<package name="CHIPLED-0603-TTW">
<description>&lt;b&gt;CHIPLED-0603&lt;/b&gt;&lt;p&gt;
Recommended Solder Pad useable for SmartLEDTM and Chipled - Package 0603&lt;br&gt;
Package able to withstand TTW-soldering heat&lt;br&gt;
Package suitable for TTW-soldering&lt;br&gt;
Source: http://www.osram.convergy.de/ ... LO_LS_LY L89K.pdf</description>
<wire x1="-0.3" y1="0.8" x2="0.3" y2="0.8" width="0.1016" layer="51" curve="170.055574"/>
<wire x1="-0.275" y1="-0.825" x2="0.275" y2="-0.825" width="0.0508" layer="51" curve="-180"/>
<wire x1="-0.4" y1="0.375" x2="-0.4" y2="-0.35" width="0.1016" layer="51"/>
<wire x1="0.4" y1="0.35" x2="0.4" y2="-0.35" width="0.1016" layer="51"/>
<circle x="-0.35" y="0.625" radius="0.075" width="0.0508" layer="51"/>
<smd name="C" x="0" y="0.875" dx="0.8" dy="0.5" layer="1" stop="no" cream="no"/>
<smd name="A" x="0" y="-0.875" dx="0.8" dy="0.5" layer="1" stop="no" cream="no"/>
<smd name="A@1" x="0" y="-0.5" dx="0.35" dy="0.35" layer="1"/>
<smd name="C@1" x="0" y="0.5" dx="0.35" dy="0.35" layer="1"/>
<text x="-0.635" y="-1.27" size="1.27" layer="25" rot="R90">&gt;NAME</text>
<text x="1.905" y="-1.27" size="1.27" layer="27" rot="R90">&gt;VALUE</text>
<rectangle x1="-0.45" y1="0.7" x2="-0.25" y2="0.85" layer="51"/>
<rectangle x1="-0.275" y1="0.55" x2="-0.225" y2="0.6" layer="51"/>
<rectangle x1="-0.45" y1="0.35" x2="-0.4" y2="0.725" layer="51"/>
<rectangle x1="0.25" y1="0.55" x2="0.45" y2="0.85" layer="51"/>
<rectangle x1="-0.45" y1="0.35" x2="0.45" y2="0.575" layer="51"/>
<rectangle x1="-0.45" y1="-0.85" x2="-0.25" y2="-0.35" layer="51"/>
<rectangle x1="0.25" y1="-0.85" x2="0.45" y2="-0.35" layer="51"/>
<rectangle x1="-0.275" y1="-0.575" x2="0.275" y2="-0.35" layer="51"/>
<rectangle x1="-0.275" y1="-0.65" x2="-0.175" y2="-0.55" layer="51"/>
<rectangle x1="0.175" y1="-0.65" x2="0.275" y2="-0.55" layer="51"/>
<rectangle x1="-0.125" y1="0" x2="0.125" y2="0.25" layer="21"/>
<rectangle x1="-0.175" y1="0.325" x2="0.175" y2="0.7" layer="29"/>
<rectangle x1="-0.4" y1="0.625" x2="0.4" y2="1.125" layer="29"/>
<rectangle x1="-0.4" y1="-1.125" x2="0.4" y2="-0.625" layer="29"/>
<rectangle x1="-0.175" y1="-0.675" x2="0.175" y2="-0.325" layer="29"/>
</package>
<package name="SMARTLED-TTW">
<description>&lt;b&gt;SmartLED TTW&lt;/b&gt;&lt;p&gt;
Recommended Solder Pad useable for SmartLEDTM and Chipled - Package 0603&lt;br&gt;
Package able to withstand TTW-soldering heat&lt;br&gt;
Package suitable for TTW-soldering&lt;br&gt;
Source: http://www.osram.convergy.de/ ... LO_LS_LY L89K.pdf</description>
<wire x1="-0.35" y1="0.6" x2="0.35" y2="0.6" width="0.1016" layer="51" style="shortdash"/>
<wire x1="0.35" y1="0.6" x2="0.35" y2="-0.6" width="0.1016" layer="21" style="shortdash"/>
<wire x1="0.35" y1="-0.6" x2="0.15" y2="-0.6" width="0.1016" layer="51" style="shortdash"/>
<wire x1="0.15" y1="-0.6" x2="-0.35" y2="-0.6" width="0.1016" layer="51" style="shortdash"/>
<wire x1="-0.35" y1="-0.6" x2="-0.35" y2="0.6" width="0.1016" layer="21" style="shortdash"/>
<wire x1="0.35" y1="-0.4" x2="0.15" y2="-0.6" width="0.1016" layer="51" style="shortdash"/>
<smd name="C" x="0" y="0.875" dx="0.8" dy="0.5" layer="1" stop="no" cream="no"/>
<smd name="A" x="0" y="-0.875" dx="0.8" dy="0.5" layer="1" stop="no" cream="no"/>
<smd name="A@1" x="0" y="-0.5" dx="0.35" dy="0.35" layer="1" stop="no" cream="no"/>
<smd name="C@1" x="0" y="0.5" dx="0.35" dy="0.35" layer="1" stop="no" cream="no"/>
<text x="-0.635" y="-1.27" size="1.27" layer="25" rot="R90">&gt;NAME</text>
<text x="1.905" y="-1.27" size="1.27" layer="27" rot="R90">&gt;VALUE</text>
<rectangle x1="-0.175" y1="0.325" x2="0.175" y2="0.7" layer="29"/>
<rectangle x1="-0.15" y1="-0.35" x2="0.15" y2="-0.05" layer="21"/>
<rectangle x1="-0.15" y1="0.6" x2="0.15" y2="0.85" layer="51"/>
<rectangle x1="-0.15" y1="-0.85" x2="0.15" y2="-0.6" layer="51"/>
<rectangle x1="-0.225" y1="0.3" x2="0.225" y2="0.975" layer="31"/>
<rectangle x1="-0.175" y1="-0.7" x2="0.175" y2="-0.325" layer="29" rot="R180"/>
<rectangle x1="-0.225" y1="-0.975" x2="0.225" y2="-0.3" layer="31" rot="R180"/>
</package>
<package name="LUMILED+">
<description>&lt;b&gt;Lumileds Lighting. LUXEON®&lt;/b&gt; with cool pad&lt;p&gt;
Source: K2.pdf</description>
<wire x1="-3.575" y1="2.3375" x2="-2.3375" y2="3.575" width="0.2032" layer="21"/>
<wire x1="-2.3375" y1="3.575" x2="2.3375" y2="3.575" width="0.2032" layer="21"/>
<wire x1="3.575" y1="2.3375" x2="3.575" y2="-3.575" width="0.2032" layer="21"/>
<wire x1="3.575" y1="-3.575" x2="-2.3375" y2="-3.575" width="0.2032" layer="21"/>
<wire x1="-2.3375" y1="-3.575" x2="-2.5" y2="-3.4125" width="0.2032" layer="21"/>
<wire x1="-2.5" y1="-3.4125" x2="-3.4125" y2="-2.5" width="0.2032" layer="21" curve="167.429893"/>
<wire x1="-3.4125" y1="-2.5" x2="-3.575" y2="-2.3375" width="0.2032" layer="21"/>
<wire x1="-3.575" y1="-2.3375" x2="-3.575" y2="2.3375" width="0.2032" layer="21"/>
<wire x1="2.3375" y1="3.575" x2="2.5" y2="3.4125" width="0.2032" layer="21"/>
<wire x1="2.5" y1="3.4125" x2="3.4125" y2="2.5" width="0.2032" layer="21" curve="167.429893"/>
<wire x1="3.4125" y1="2.5" x2="3.575" y2="2.3375" width="0.2032" layer="21"/>
<wire x1="-1.725" y1="2.225" x2="-1.0625" y2="2.5625" width="0.2032" layer="21" curve="-255.44999"/>
<wire x1="1.725" y1="-2.225" x2="1.0625" y2="-2.5625" width="0.2032" layer="21" curve="-255.44999"/>
<circle x="0" y="0" radius="2.725" width="0.2032" layer="51"/>
<smd name="1NC" x="-5.2" y="1.15" dx="2.9" dy="1.7" layer="1"/>
<smd name="2+" x="-5.2" y="-1.15" dx="2.9" dy="1.7" layer="1"/>
<smd name="3NC" x="5.2" y="-1.15" dx="2.9" dy="1.7" layer="1" rot="R180"/>
<smd name="4-" x="5.2" y="1.15" dx="2.9" dy="1.7" layer="1" rot="R180"/>
<text x="-3.175" y="3.81" size="1.27" layer="25">&gt;NAME</text>
<text x="-3.175" y="-5.08" size="1.27" layer="27">&gt;VALUE</text>
<rectangle x1="-5.975" y1="0.575" x2="-3.625" y2="1.6" layer="51"/>
<rectangle x1="-5.975" y1="-1.6" x2="-3.625" y2="-0.575" layer="51"/>
<rectangle x1="3.625" y1="-1.6" x2="5.975" y2="-0.575" layer="51" rot="R180"/>
<rectangle x1="3.625" y1="0.575" x2="5.975" y2="1.6" layer="51" rot="R180"/>
<polygon width="0.4064" layer="1">
<vertex x="2.3383" y="1.35"/>
<vertex x="0" y="2.7"/>
<vertex x="-2.3383" y="1.35"/>
<vertex x="-2.3383" y="-1.35"/>
<vertex x="0" y="-2.7"/>
<vertex x="2.3383" y="-1.35"/>
</polygon>
<polygon width="0.4064" layer="29">
<vertex x="2.3383" y="1.35"/>
<vertex x="0" y="2.7"/>
<vertex x="-2.3383" y="1.35"/>
<vertex x="-2.3383" y="-1.35"/>
<vertex x="0" y="-2.7"/>
<vertex x="2.3383" y="-1.35"/>
</polygon>
<polygon width="0.4064" layer="31">
<vertex x="2.3383" y="1.35"/>
<vertex x="0" y="2.7"/>
<vertex x="-2.3383" y="1.35"/>
<vertex x="-2.3383" y="-1.35"/>
<vertex x="0" y="-2.7"/>
<vertex x="2.3383" y="-1.35"/>
</polygon>
</package>
<package name="LUMILED">
<description>&lt;b&gt;Lumileds Lighting. LUXEON®&lt;/b&gt; without cool pad&lt;p&gt;
Source: K2.pdf</description>
<wire x1="-3.575" y1="2.3375" x2="-2.3375" y2="3.575" width="0.2032" layer="21"/>
<wire x1="-2.3375" y1="3.575" x2="2.3375" y2="3.575" width="0.2032" layer="21"/>
<wire x1="3.575" y1="2.3375" x2="3.575" y2="-3.575" width="0.2032" layer="21"/>
<wire x1="3.575" y1="-3.575" x2="-2.3375" y2="-3.575" width="0.2032" layer="21"/>
<wire x1="-2.3375" y1="-3.575" x2="-2.5" y2="-3.4125" width="0.2032" layer="21"/>
<wire x1="-2.5" y1="-3.4125" x2="-3.4125" y2="-2.5" width="0.2032" layer="21" curve="167.429893"/>
<wire x1="-3.4125" y1="-2.5" x2="-3.575" y2="-2.3375" width="0.2032" layer="21"/>
<wire x1="-3.575" y1="-2.3375" x2="-3.575" y2="2.3375" width="0.2032" layer="21"/>
<wire x1="2.3375" y1="3.575" x2="2.5" y2="3.4125" width="0.2032" layer="21"/>
<wire x1="2.5" y1="3.4125" x2="3.4125" y2="2.5" width="0.2032" layer="21" curve="167.429893"/>
<wire x1="3.4125" y1="2.5" x2="3.575" y2="2.3375" width="0.2032" layer="21"/>
<wire x1="-1.725" y1="2.225" x2="-1.0625" y2="2.5625" width="0.2032" layer="21" curve="-255.44999"/>
<wire x1="1.725" y1="-2.225" x2="1.0625" y2="-2.5625" width="0.2032" layer="21" curve="-255.44999"/>
<circle x="0" y="0" radius="2.725" width="0.2032" layer="51"/>
<smd name="1NC" x="-5.2" y="1.15" dx="2.9" dy="1.7" layer="1"/>
<smd name="2+" x="-5.2" y="-1.15" dx="2.9" dy="1.7" layer="1"/>
<smd name="3NC" x="5.2" y="-1.15" dx="2.9" dy="1.7" layer="1" rot="R180"/>
<smd name="4-" x="5.2" y="1.15" dx="2.9" dy="1.7" layer="1" rot="R180"/>
<text x="-3.175" y="3.81" size="1.27" layer="25">&gt;NAME</text>
<text x="-3.175" y="-5.08" size="1.27" layer="27">&gt;VALUE</text>
<rectangle x1="-5.975" y1="0.575" x2="-3.625" y2="1.6" layer="51"/>
<rectangle x1="-5.975" y1="-1.6" x2="-3.625" y2="-0.575" layer="51"/>
<rectangle x1="3.625" y1="-1.6" x2="5.975" y2="-0.575" layer="51" rot="R180"/>
<rectangle x1="3.625" y1="0.575" x2="5.975" y2="1.6" layer="51" rot="R180"/>
<polygon width="0.4064" layer="29">
<vertex x="2.3383" y="1.35"/>
<vertex x="0" y="2.7"/>
<vertex x="-2.3383" y="1.35"/>
<vertex x="-2.3383" y="-1.35"/>
<vertex x="0" y="-2.7"/>
<vertex x="2.3383" y="-1.35"/>
</polygon>
<polygon width="0.4064" layer="31">
<vertex x="2.3383" y="1.35"/>
<vertex x="0" y="2.7"/>
<vertex x="-2.3383" y="1.35"/>
<vertex x="-2.3383" y="-1.35"/>
<vertex x="0" y="-2.7"/>
<vertex x="2.3383" y="-1.35"/>
</polygon>
</package>
<package name="LED10MM">
<description>&lt;B&gt;LED&lt;/B&gt;&lt;p&gt;
10 mm, round</description>
<wire x1="5.08" y1="-2.54" x2="5.08" y2="2.54" width="0.254" layer="21" curve="-306.869898"/>
<wire x1="4.445" y1="0" x2="0" y2="-4.445" width="0.127" layer="21" curve="-90"/>
<wire x1="3.81" y1="0" x2="0" y2="-3.81" width="0.127" layer="21" curve="-90"/>
<wire x1="3.175" y1="0" x2="0" y2="-3.175" width="0.127" layer="21" curve="-90"/>
<wire x1="2.54" y1="0" x2="0" y2="-2.54" width="0.127" layer="21" curve="-90"/>
<wire x1="-4.445" y1="0" x2="0" y2="4.445" width="0.127" layer="21" curve="-90"/>
<wire x1="-3.81" y1="0" x2="0" y2="3.81" width="0.127" layer="21" curve="-90"/>
<wire x1="-3.175" y1="0" x2="0" y2="3.175" width="0.127" layer="21" curve="-90"/>
<wire x1="-2.54" y1="0" x2="0" y2="2.54" width="0.127" layer="21" curve="-90"/>
<wire x1="5.08" y1="2.54" x2="5.08" y2="-2.54" width="0.254" layer="21"/>
<circle x="0" y="0" radius="5.08" width="0.127" layer="21"/>
<pad name="K" x="1.27" y="0" drill="0.8128" diameter="1.6764" shape="square"/>
<pad name="A" x="-1.27" y="0" drill="0.8128" diameter="1.6764" shape="octagon"/>
<text x="6.35" y="1.27" size="1.27" layer="25">&gt;NAME</text>
<text x="6.35" y="-1.27" size="1.27" layer="27">&gt;VALUE</text>
</package>
<package name="KA-3528ASYC">
<description>&lt;b&gt;SURFACE MOUNT LED LAMP&lt;/b&gt; 3.5x2.8mm&lt;p&gt;
Source: http://www.kingbright.com/manager/upload/pdf/KA-3528ASYC(Ver1189474662.1)</description>
<wire x1="-1.55" y1="1.35" x2="1.55" y2="1.35" width="0.1016" layer="21"/>
<wire x1="1.55" y1="1.35" x2="1.55" y2="-1.35" width="0.1016" layer="51"/>
<wire x1="1.55" y1="-1.35" x2="-1.55" y2="-1.35" width="0.1016" layer="21"/>
<wire x1="-1.55" y1="-1.35" x2="-1.55" y2="1.35" width="0.1016" layer="51"/>
<wire x1="-0.65" y1="0.95" x2="0.65" y2="0.95" width="0.1016" layer="21" curve="-68.40813"/>
<wire x1="0.65" y1="-0.95" x2="-0.65" y2="-0.95" width="0.1016" layer="21" curve="-68.40813"/>
<circle x="0" y="0" radius="1.15" width="0.1016" layer="51"/>
<smd name="A" x="-1.55" y="0" dx="1.5" dy="2.2" layer="1"/>
<smd name="C" x="1.55" y="0" dx="1.5" dy="2.2" layer="1"/>
<text x="-1.905" y="1.905" size="1.27" layer="25">&gt;NAME</text>
<text x="-1.905" y="-3.175" size="1.27" layer="27">&gt;VALUE</text>
<rectangle x1="-1.75" y1="0.6" x2="-1.6" y2="1.1" layer="51"/>
<rectangle x1="-1.75" y1="-1.1" x2="-1.6" y2="-0.6" layer="51"/>
<rectangle x1="1.6" y1="-1.1" x2="1.75" y2="-0.6" layer="51" rot="R180"/>
<rectangle x1="1.6" y1="0.6" x2="1.75" y2="1.1" layer="51" rot="R180"/>
<polygon width="0.1016" layer="51">
<vertex x="1.55" y="-1.35"/>
<vertex x="1.55" y="-0.625"/>
<vertex x="0.825" y="-1.35"/>
</polygon>
<polygon width="0.1016" layer="21">
<vertex x="1.55" y="-1.35"/>
<vertex x="1.55" y="-1.175"/>
<vertex x="1" y="-1.175"/>
<vertex x="0.825" y="-1.35"/>
</polygon>
</package>
<package name="SML0805">
<description>&lt;b&gt;SML0805-2CW-TR (0805 PROFILE)&lt;/b&gt; COOL WHITE&lt;p&gt;
Source: http://www.ledtronics.com/ds/smd-0603/Dstr0093.pdf</description>
<wire x1="-0.95" y1="-0.55" x2="0.95" y2="-0.55" width="0.1016" layer="51"/>
<wire x1="0.95" y1="-0.55" x2="0.95" y2="0.55" width="0.1016" layer="51"/>
<wire x1="0.95" y1="0.55" x2="-0.95" y2="0.55" width="0.1016" layer="51"/>
<wire x1="-0.95" y1="0.55" x2="-0.95" y2="-0.55" width="0.1016" layer="51"/>
<wire x1="-0.175" y1="-0.025" x2="0" y2="0.15" width="0.0634" layer="21"/>
<wire x1="0" y1="0.15" x2="0.15" y2="0" width="0.0634" layer="21"/>
<wire x1="0.15" y1="0" x2="-0.025" y2="-0.175" width="0.0634" layer="21"/>
<wire x1="-0.025" y1="-0.175" x2="-0.175" y2="-0.025" width="0.0634" layer="21"/>
<circle x="-0.275" y="0.4" radius="0.125" width="0" layer="21"/>
<smd name="C" x="-1.05" y="0" dx="1.2" dy="1.2" layer="1"/>
<smd name="A" x="1.05" y="0" dx="1.2" dy="1.2" layer="1"/>
<text x="-1.5" y="1" size="1.27" layer="25">&gt;NAME</text>
<text x="-1.5" y="-2" size="1.27" layer="27">&gt;VALUE</text>
</package>
<package name="SML1206">
<description>&lt;b&gt;SML10XXKH-TR (HIGH INTENSITY) LED&lt;/b&gt;&lt;p&gt;
&lt;table&gt;
&lt;tr&gt;&lt;td&gt;SML10R3KH-TR&lt;/td&gt;&lt;td&gt;ULTRA RED&lt;/td&gt;&lt;/tr&gt;
&lt;tr&gt;&lt;td&gt;SML10E3KH-TR&lt;/td&gt;&lt;td&gt;SUPER REDSUPER BLUE&lt;/td&gt;&lt;/tr&gt;
&lt;tr&gt;&lt;td&gt;SML10O3KH-TR&lt;/td&gt;&lt;td&gt;SUPER ORANGE&lt;/td&gt;&lt;/tr&gt;
&lt;tr&gt;&lt;td&gt;SML10PY3KH-TR&lt;/td&gt;&lt;td&gt;PURE YELLOW&lt;/td&gt;&lt;/tr&gt;
&lt;tr&gt;&lt;td&gt;SML10OY3KH-TR&lt;/td&gt;&lt;td&gt;ULTRA YELLOW&lt;/td&gt;&lt;/tr&gt;
&lt;tr&gt;&lt;td&gt;SML10AG3KH-TR&lt;/td&gt;&lt;td&gt;AQUA GREEN&lt;/td&gt;&lt;/tr&gt;
&lt;tr&gt;&lt;td&gt;SML10BG3KH-TR&lt;/td&gt;&lt;td&gt;BLUE GREEN&lt;/td&gt;&lt;/tr&gt;
&lt;tr&gt;&lt;td&gt;SML10PB1KH-TR&lt;/td&gt;&lt;td&gt;SUPER BLUE&lt;/td&gt;&lt;/tr&gt;
&lt;tr&gt;&lt;td&gt;SML10CW1KH-TR&lt;/td&gt;&lt;td&gt;WHITE&lt;/td&gt;&lt;/tr&gt;
&lt;/table&gt;

Source: http://www.ledtronics.com/ds/smd-1206/dstr0094.PDF</description>
<wire x1="-1.5" y1="0.5" x2="-1.5" y2="-0.5" width="0.2032" layer="51" curve="-180"/>
<wire x1="1.5" y1="-0.5" x2="1.5" y2="0.5" width="0.2032" layer="51" curve="-180"/>
<wire x1="-1.55" y1="0.75" x2="1.55" y2="0.75" width="0.1016" layer="51"/>
<wire x1="1.55" y1="-0.75" x2="-1.55" y2="-0.75" width="0.1016" layer="51"/>
<circle x="-0.725" y="0.525" radius="0.125" width="0" layer="21"/>
<smd name="C" x="-1.75" y="0" dx="1.5" dy="1.5" layer="1"/>
<smd name="A" x="1.75" y="0" dx="1.5" dy="1.5" layer="1"/>
<text x="-1.5" y="1" size="1.27" layer="25">&gt;NAME</text>
<text x="-1.5" y="-2.5" size="1.27" layer="27">&gt;VALUE</text>
<rectangle x1="-1.6" y1="0.4" x2="-1.15" y2="0.8" layer="51"/>
<rectangle x1="-1.6" y1="-0.8" x2="-1.15" y2="-0.4" layer="51"/>
<rectangle x1="-1.175" y1="-0.6" x2="-1" y2="-0.275" layer="51"/>
<rectangle x1="1.15" y1="-0.8" x2="1.6" y2="-0.4" layer="51" rot="R180"/>
<rectangle x1="1.15" y1="0.4" x2="1.6" y2="0.8" layer="51" rot="R180"/>
<rectangle x1="1" y1="0.275" x2="1.175" y2="0.6" layer="51" rot="R180"/>
<rectangle x1="-0.1" y1="-0.1" x2="0.1" y2="0.1" layer="21"/>
</package>
<package name="SML0603">
<description>&lt;b&gt;SML0603-XXX (HIGH INTENSITY) LED&lt;/b&gt;&lt;p&gt;
&lt;table&gt;
&lt;tr&gt;&lt;td&gt;AG3K&lt;/td&gt;&lt;td&gt;AQUA GREEN&lt;/td&gt;&lt;/tr&gt;
&lt;tr&gt;&lt;td&gt;B1K&lt;/td&gt;&lt;td&gt;SUPER BLUE&lt;/td&gt;&lt;/tr&gt;
&lt;tr&gt;&lt;td&gt;R1K&lt;/td&gt;&lt;td&gt;SUPER RED&lt;/td&gt;&lt;/tr&gt;
&lt;tr&gt;&lt;td&gt;R3K&lt;/td&gt;&lt;td&gt;ULTRA RED&lt;/td&gt;&lt;/tr&gt;
&lt;tr&gt;&lt;td&gt;O3K&lt;/td&gt;&lt;td&gt;SUPER ORANGE&lt;/td&gt;&lt;/tr&gt;
&lt;tr&gt;&lt;td&gt;O3KH&lt;/td&gt;&lt;td&gt;SOFT ORANGE&lt;/td&gt;&lt;/tr&gt;
&lt;tr&gt;&lt;td&gt;Y3KH&lt;/td&gt;&lt;td&gt;SUPER YELLOW&lt;/td&gt;&lt;/tr&gt;
&lt;tr&gt;&lt;td&gt;Y3K&lt;/td&gt;&lt;td&gt;SUPER YELLOW&lt;/td&gt;&lt;/tr&gt;
&lt;tr&gt;&lt;td&gt;2CW&lt;/td&gt;&lt;td&gt;WHITE&lt;/td&gt;&lt;/tr&gt;
&lt;/table&gt;
Source: http://www.ledtronics.com/ds/smd-0603/Dstr0092.pdf</description>
<wire x1="-0.75" y1="0.35" x2="0.75" y2="0.35" width="0.1016" layer="51"/>
<wire x1="0.75" y1="0.35" x2="0.75" y2="-0.35" width="0.1016" layer="51"/>
<wire x1="0.75" y1="-0.35" x2="-0.75" y2="-0.35" width="0.1016" layer="51"/>
<wire x1="-0.75" y1="-0.35" x2="-0.75" y2="0.35" width="0.1016" layer="51"/>
<wire x1="-0.45" y1="0.3" x2="-0.45" y2="-0.3" width="0.1016" layer="51"/>
<wire x1="0.45" y1="0.3" x2="0.45" y2="-0.3" width="0.1016" layer="51"/>
<wire x1="-0.2" y1="0.35" x2="0.2" y2="0.35" width="0.1016" layer="21"/>
<wire x1="-0.2" y1="-0.35" x2="0.2" y2="-0.35" width="0.1016" layer="21"/>
<smd name="C" x="-0.75" y="0" dx="0.8" dy="0.8" layer="1"/>
<smd name="A" x="0.75" y="0" dx="0.8" dy="0.8" layer="1"/>
<text x="-1" y="1" size="1.27" layer="25">&gt;NAME</text>
<text x="-1" y="-2" size="1.27" layer="27">&gt;VALUE</text>
<rectangle x1="-0.4" y1="0.175" x2="0" y2="0.4" layer="51"/>
<rectangle x1="-0.25" y1="0.175" x2="0" y2="0.4" layer="21"/>
</package>
</packages>
<symbols>
<symbol name="LED">
<wire x1="1.27" y1="0" x2="0" y2="-2.54" width="0.254" layer="94"/>
<wire x1="0" y1="-2.54" x2="-1.27" y2="0" width="0.254" layer="94"/>
<wire x1="1.27" y1="-2.54" x2="0" y2="-2.54" width="0.254" layer="94"/>
<wire x1="0" y1="-2.54" x2="-1.27" y2="-2.54" width="0.254" layer="94"/>
<wire x1="1.27" y1="0" x2="0" y2="0" width="0.254" layer="94"/>
<wire x1="0" y1="0" x2="-1.27" y2="0" width="0.254" layer="94"/>
<wire x1="0" y1="0" x2="0" y2="-2.54" width="0.1524" layer="94"/>
<wire x1="-2.032" y1="-0.762" x2="-3.429" y2="-2.159" width="0.1524" layer="94"/>
<wire x1="-1.905" y1="-1.905" x2="-3.302" y2="-3.302" width="0.1524" layer="94"/>
<text x="3.556" y="-1.27" size="1.778" layer="95" rot="R90" align="bottom-center">&gt;NAME</text>
<text x="5.715" y="-1.27" size="1.778" layer="96" rot="R90" align="bottom-center">&gt;VALUE</text>
<pin name="C" x="0" y="-5.08" visible="off" length="short" direction="pas" rot="R90"/>
<pin name="A" x="0" y="2.54" visible="off" length="short" direction="pas" rot="R270"/>
<polygon width="0.1524" layer="94">
<vertex x="-3.429" y="-2.159"/>
<vertex x="-3.048" y="-1.27"/>
<vertex x="-2.54" y="-1.778"/>
</polygon>
<polygon width="0.1524" layer="94">
<vertex x="-3.302" y="-3.302"/>
<vertex x="-2.921" y="-2.413"/>
<vertex x="-2.413" y="-2.921"/>
</polygon>
</symbol>
</symbols>
<devicesets>
<deviceset name="LED" prefix="LED" uservalue="yes">
<description>&lt;b&gt;LED&lt;/b&gt;&lt;p&gt;
&lt;u&gt;OSRAM&lt;/u&gt;:&lt;br&gt;

- &lt;u&gt;CHIPLED&lt;/u&gt;&lt;br&gt;
LG R971, LG N971, LY N971, LG Q971, LY Q971, LO R971, LY R971
LH N974, LH R974&lt;br&gt;
LS Q976, LO Q976, LY Q976&lt;br&gt;
LO Q996&lt;br&gt;

- &lt;u&gt;Hyper CHIPLED&lt;/u&gt;&lt;br&gt;
LW Q18S&lt;br&gt;
LB Q993, LB Q99A, LB R99A&lt;br&gt;

- &lt;u&gt;SideLED&lt;/u&gt;&lt;br&gt;
LS A670, LO A670, LY A670, LG A670, LP A670&lt;br&gt;
LB A673, LV A673, LT A673, LW A673&lt;br&gt;
LH A674&lt;br&gt;
LY A675&lt;br&gt;
LS A676, LA A676, LO A676, LY A676, LW A676&lt;br&gt;
LS A679, LY A679, LG A679&lt;br&gt;

-  &lt;u&gt;Hyper Micro SIDELED®&lt;/u&gt;&lt;br&gt;
LS Y876, LA Y876, LO Y876, LY Y876&lt;br&gt;
LT Y87S&lt;br&gt;

- &lt;u&gt;SmartLED&lt;/u&gt;&lt;br&gt;
LW L88C, LW L88S&lt;br&gt;
LB L89C, LB L89S, LG L890&lt;br&gt;
LS L89K, LO L89K, LY L89K&lt;br&gt;
LS L896, LA L896, LO L896, LY L896&lt;br&gt;

- &lt;u&gt;TOPLED&lt;/u&gt;&lt;br&gt;
LS T670, LO T670, LY T670, LG T670, LP T670&lt;br&gt;
LSG T670, LSP T670, LSY T670, LOP T670, LYG T670&lt;br&gt;
LG T671, LOG T671, LSG T671&lt;br&gt;
LB T673, LV T673, LT T673, LW T673&lt;br&gt;
LH T674&lt;br&gt;
LS T676, LA T676, LO T676, LY T676, LB T676, LH T676, LSB T676, LW T676&lt;br&gt;
LB T67C, LV T67C, LT T67C, LS T67K, LO T67K, LY T67K, LW E67C&lt;br&gt;
LS E67B, LA E67B, LO E67B, LY E67B, LB E67C, LV E67C, LT E67C&lt;br&gt;
LW T67C&lt;br&gt;
LS T679, LY T679, LG T679&lt;br&gt;
LS T770, LO T770, LY T770, LG T770, LP T770&lt;br&gt;
LB T773, LV T773, LT T773, LW T773&lt;br&gt;
LH T774&lt;br&gt;
LS E675, LA E675, LY E675, LS T675&lt;br&gt;
LS T776, LA T776, LO T776, LY T776, LB T776&lt;br&gt;
LHGB T686&lt;br&gt;
LT T68C, LB T68C&lt;br&gt;

- &lt;u&gt;Hyper Mini TOPLED®&lt;/u&gt;&lt;br&gt;
LB M676&lt;br&gt;

- &lt;u&gt;Mini TOPLED Santana®&lt;/u&gt;&lt;br&gt;
LG M470&lt;br&gt;
LS M47K, LO M47K, LY M47K
&lt;p&gt;
Source: http://www.osram.convergy.de&lt;p&gt;

&lt;u&gt;LUXEON:&lt;/u&gt;&lt;br&gt;
- &lt;u&gt;LUMILED®&lt;/u&gt;&lt;br&gt;
LXK2-PW12-R00, LXK2-PW12-S00, LXK2-PW14-U00, LXK2-PW14-V00&lt;br&gt;
LXK2-PM12-R00, LXK2-PM12-S00, LXK2-PM14-U00&lt;br&gt;
LXK2-PE12-Q00, LXK2-PE12-R00, LXK2-PE12-S00, LXK2-PE14-T00, LXK2-PE14-U00&lt;br&gt;
LXK2-PB12-K00, LXK2-PB12-L00, LXK2-PB12-M00, LXK2-PB14-N00, LXK2-PB14-P00, LXK2-PB14-Q00&lt;br&gt;
LXK2-PR12-L00, LXK2-PR12-M00, LXK2-PR14-Q00, LXK2-PR14-R00&lt;br&gt;
LXK2-PD12-Q00, LXK2-PD12-R00, LXK2-PD12-S00&lt;br&gt;
LXK2-PH12-R00, LXK2-PH12-S00&lt;br&gt;
LXK2-PL12-P00, LXK2-PL12-Q00, LXK2-PL12-R00
&lt;p&gt;
Source: www.luxeon.com&lt;p&gt;

&lt;u&gt;KINGBRIGHT:&lt;/U&gt;&lt;p&gt;
KA-3528ASYC&lt;br&gt;
Source: www.kingbright.com</description>
<gates>
<gate name="G$1" symbol="LED" x="0" y="0"/>
</gates>
<devices>
<device name="SMT1206" package="1206">
<connects>
<connect gate="G$1" pin="A" pad="A"/>
<connect gate="G$1" pin="C" pad="C"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="LD260" package="LD260">
<connects>
<connect gate="G$1" pin="A" pad="A"/>
<connect gate="G$1" pin="C" pad="K"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="SQR2X5" package="LED2X5">
<connects>
<connect gate="G$1" pin="A" pad="A"/>
<connect gate="G$1" pin="C" pad="K"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="3MM" package="LED3MM">
<connects>
<connect gate="G$1" pin="A" pad="A"/>
<connect gate="G$1" pin="C" pad="K"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="5MM" package="LED5MM">
<connects>
<connect gate="G$1" pin="A" pad="A"/>
<connect gate="G$1" pin="C" pad="K"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="LSU260" package="LSU260">
<connects>
<connect gate="G$1" pin="A" pad="A"/>
<connect gate="G$1" pin="C" pad="K"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="LZR181" package="LZR181">
<connects>
<connect gate="G$1" pin="A" pad="A"/>
<connect gate="G$1" pin="C" pad="K"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="B152" package="Q62902-B152">
<connects>
<connect gate="G$1" pin="A" pad="A"/>
<connect gate="G$1" pin="C" pad="K"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="B153" package="Q62902-B153">
<connects>
<connect gate="G$1" pin="A" pad="A"/>
<connect gate="G$1" pin="C" pad="K"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="B155" package="Q62902-B155">
<connects>
<connect gate="G$1" pin="A" pad="A"/>
<connect gate="G$1" pin="C" pad="K"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="B156" package="Q62902-B156">
<connects>
<connect gate="G$1" pin="A" pad="A"/>
<connect gate="G$1" pin="C" pad="K"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="SFH480" package="SFH480">
<connects>
<connect gate="G$1" pin="A" pad="A"/>
<connect gate="G$1" pin="C" pad="K"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="SFH482" package="SFH482">
<connects>
<connect gate="G$1" pin="A" pad="A"/>
<connect gate="G$1" pin="C" pad="K"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="SQR5.7X3.2" package="U57X32">
<connects>
<connect gate="G$1" pin="A" pad="A"/>
<connect gate="G$1" pin="C" pad="K"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="IRL80A" package="IRL80A">
<connects>
<connect gate="G$1" pin="A" pad="A"/>
<connect gate="G$1" pin="C" pad="K"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="P-LCC-2" package="P-LCC-2">
<connects>
<connect gate="G$1" pin="A" pad="A"/>
<connect gate="G$1" pin="C" pad="C"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="MINI-TOP" package="OSRAM-MINI-TOP-LED">
<connects>
<connect gate="G$1" pin="A" pad="A"/>
<connect gate="G$1" pin="C" pad="C"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="SIDELED" package="OSRAM-SIDELED">
<connects>
<connect gate="G$1" pin="A" pad="A"/>
<connect gate="G$1" pin="C" pad="C"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="SMART-LED" package="SMART-LED">
<connects>
<connect gate="G$1" pin="A" pad="A"/>
<connect gate="G$1" pin="C" pad="B"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="P-LCC-2-BACK" package="P-LCC-2-TOPLED-RG">
<connects>
<connect gate="G$1" pin="A" pad="A"/>
<connect gate="G$1" pin="C" pad="C"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="MICRO-SIDELED" package="MICRO-SIDELED">
<connects>
<connect gate="G$1" pin="A" pad="A"/>
<connect gate="G$1" pin="C" pad="C"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="P-LCC-4" package="P-LCC-4">
<connects>
<connect gate="G$1" pin="A" pad="A"/>
<connect gate="G$1" pin="C" pad="C@4"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="CHIP-LED0603" package="CHIP-LED0603">
<connects>
<connect gate="G$1" pin="A" pad="A"/>
<connect gate="G$1" pin="C" pad="C"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="CHIP-LED0805" package="CHIP-LED0805">
<connects>
<connect gate="G$1" pin="A" pad="A"/>
<connect gate="G$1" pin="C" pad="C"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="TOPLED-SANTANA" package="MINI-TOPLED-SANTANA">
<connects>
<connect gate="G$1" pin="A" pad="A"/>
<connect gate="G$1" pin="C" pad="C"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="CHIPLED_0805" package="CHIPLED_0805">
<connects>
<connect gate="G$1" pin="A" pad="A"/>
<connect gate="G$1" pin="C" pad="C"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="CHIPLED_1206" package="CHIPLED_1206">
<connects>
<connect gate="G$1" pin="A" pad="A"/>
<connect gate="G$1" pin="C" pad="C"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="CHIPLED_0603" package="CHIPLED_0603">
<connects>
<connect gate="G$1" pin="A" pad="A"/>
<connect gate="G$1" pin="C" pad="C"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="CHIPLED-0603-TTW" package="CHIPLED-0603-TTW">
<connects>
<connect gate="G$1" pin="A" pad="A@1"/>
<connect gate="G$1" pin="C" pad="C@1"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="" package="SMARTLED-TTW">
<connects>
<connect gate="G$1" pin="A" pad="A@1"/>
<connect gate="G$1" pin="C" pad="C@1"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="-LUMILED+" package="LUMILED+">
<connects>
<connect gate="G$1" pin="A" pad="2+"/>
<connect gate="G$1" pin="C" pad="4-"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="-LUMILED" package="LUMILED">
<connects>
<connect gate="G$1" pin="A" pad="2+"/>
<connect gate="G$1" pin="C" pad="4-"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="10MM" package="LED10MM">
<connects>
<connect gate="G$1" pin="A" pad="A"/>
<connect gate="G$1" pin="C" pad="K"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="KA-3528ASYC" package="KA-3528ASYC">
<connects>
<connect gate="G$1" pin="A" pad="A"/>
<connect gate="G$1" pin="C" pad="C"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="SML0805" package="SML0805">
<connects>
<connect gate="G$1" pin="A" pad="A"/>
<connect gate="G$1" pin="C" pad="C"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="SML1206" package="SML1206">
<connects>
<connect gate="G$1" pin="A" pad="A"/>
<connect gate="G$1" pin="C" pad="C"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
<device name="SML0603" package="SML0603">
<connects>
<connect gate="G$1" pin="A" pad="A"/>
<connect gate="G$1" pin="C" pad="C"/>
</connects>
<technologies>
<technology name=""/>
</technologies>
</device>
</devices>
</deviceset>
</devicesets>
</library>
</libraries>
<attributes>
<attribute name="AUTHOR" value="Nathan Campos"/>
<attribute name="REVISION" value="A"/>
</attributes>
<variantdefs>
</variantdefs>
<classes>
<class number="0" name="default" width="0" drill="0">
</class>
</classes>
<parts>
<part name="FRAME1" library="innove-frames" deviceset="A4L" device=""/>
<part name="Q3" library="innove-bjts" deviceset="BC547" device=""/>
<part name="R5" library="innove-passives" deviceset="R" device="T0207" value="6k8"/>
<part name="R3" library="innove-passives" deviceset="R" device="T0207" value="10k"/>
<part name="C3" library="innove-passives" deviceset="CPOL" device="-E1.8-4" value="4u7"/>
<part name="R6" library="innove-passives" deviceset="R" device="T0207" value="1k"/>
<part name="R2" library="innove-passives" deviceset="R" device="T0207" value="100k"/>
<part name="GND6" library="innove-supply" deviceset="GND" device=""/>
<part name="GND8" library="innove-supply" deviceset="GND" device=""/>
<part name="GND9" library="innove-supply" deviceset="GND" device=""/>
<part name="Q2" library="innove-bjts" deviceset="BC559" device=""/>
<part name="Q1" library="innove-bjts" deviceset="BC559" device=""/>
<part name="R4" library="innove-passives" deviceset="R" device="T0207" value="10k"/>
<part name="R7" library="innove-passives" deviceset="R" device="T0207" value="330"/>
<part name="GND7" library="innove-supply" deviceset="GND" device=""/>
<part name="+12V6" library="innove-supply" deviceset="+12V" device=""/>
<part name="+12V5" library="innove-supply" deviceset="+12V" device=""/>
<part name="C1" library="innove-passives" deviceset="C" device="5.0-2.5X7.5" value="100n"/>
<part name="+12V2" library="innove-supply" deviceset="+12V" device=""/>
<part name="GND2" library="innove-supply" deviceset="GND" device=""/>
<part name="C2" library="innove-passives" deviceset="CPOL" device="-E5-10" value="470u"/>
<part name="GND3" library="innove-supply" deviceset="GND" device=""/>
<part name="+12V3" library="innove-supply" deviceset="+12V" device=""/>
<part name="C5" library="innove-passives" deviceset="CPOL" device="-E5-10" value="1000u"/>
<part name="Q4" library="innove-bjts" deviceset="BC547" device=""/>
<part name="Q6" library="innove-bjts" deviceset="BD140" device=""/>
<part name="R9" library="innove-passives" deviceset="R" device="T0207" value="1k"/>
<part name="+12V7" library="innove-supply" deviceset="+12V" device=""/>
<part name="+12V9" library="innove-supply" deviceset="+12V" device=""/>
<part name="Q7" library="innove-bjts" deviceset="BD139" device=""/>
<part name="Q5" library="innove-bjts" deviceset="BC547" device=""/>
<part name="R10" library="innove-passives" deviceset="R" device="T0207" value="4k7"/>
<part name="+12V8" library="innove-supply" deviceset="+12V" device=""/>
<part name="GND10" library="innove-supply" deviceset="GND" device=""/>
<part name="R11" library="innove-passives" deviceset="R" device="T0207" value="10"/>
<part name="GND11" library="innove-supply" deviceset="GND" device=""/>
<part name="R8" library="innove-passives" deviceset="R" device="T0207" value="68k"/>
<part name="C4" library="innove-passives" deviceset="C" device="5.0-2.5X7.5" value="10p"/>
<part name="R12" library="innove-passives" deviceset="R" device="T0207" value="100k"/>
<part name="GND12" library="innove-supply" deviceset="GND" device=""/>
<part name="CN2" library="innove-con-jst-xh" deviceset="B2B-XH-A" device="" value="Input"/>
<part name="CN3" library="innove-con-jst-xh" deviceset="B2B-XH-A" device="" value="Output"/>
<part name="CN1" library="innove-con-jst-xh" deviceset="B2B-XH-A" device="" value="Power"/>
<part name="+12V1" library="innove-supply" deviceset="+12V" device=""/>
<part name="GND1" library="innove-supply" deviceset="GND" device=""/>
<part name="LED1" library="innove-leds" deviceset="LED" device="5MM" value="Blue"/>
<part name="R1" library="innove-passives" deviceset="R" device="T0207" value="1k"/>
<part name="+12V4" library="innove-supply" deviceset="+12V" device=""/>
<part name="GND4" library="innove-supply" deviceset="GND" device=""/>
<part name="GND5" library="innove-supply" deviceset="GND" device=""/>
<part name="GND13" library="innove-supply" deviceset="GND" device=""/>
</parts>
<sheets>
<sheet>
<plain>
</plain>
<instances>
<instance part="FRAME1" gate="G$1" x="0" y="0"/>
<instance part="Q3" gate="G$1" x="114.3" y="76.2"/>
<instance part="R5" gate="G$1" x="106.68" y="68.58" rot="R90"/>
<instance part="R3" gate="G$1" x="96.52" y="76.2"/>
<instance part="C3" gate="G$1" x="86.36" y="76.2" rot="R270"/>
<instance part="R6" gate="G$1" x="106.68" y="58.42" rot="R90"/>
<instance part="R2" gate="G$1" x="76.2" y="68.58" rot="R90"/>
<instance part="GND6" gate="1" x="76.2" y="60.96"/>
<instance part="GND8" gate="1" x="106.68" y="50.8"/>
<instance part="GND9" gate="1" x="116.84" y="66.04"/>
<instance part="Q2" gate="G$1" x="114.3" y="111.76"/>
<instance part="Q1" gate="G$1" x="109.22" y="119.38" rot="MR0"/>
<instance part="R4" gate="G$1" x="106.68" y="104.14" rot="R90"/>
<instance part="R7" gate="G$1" x="116.84" y="127" rot="R90"/>
<instance part="GND7" gate="1" x="106.68" y="96.52"/>
<instance part="+12V6" gate="G$1" x="116.84" y="134.62"/>
<instance part="+12V5" gate="G$1" x="106.68" y="127"/>
<instance part="C1" gate="G$1" x="43.18" y="149.86"/>
<instance part="+12V2" gate="G$1" x="43.18" y="157.48"/>
<instance part="GND2" gate="1" x="43.18" y="139.7"/>
<instance part="C2" gate="G$1" x="53.34" y="149.86"/>
<instance part="GND3" gate="1" x="53.34" y="139.7"/>
<instance part="+12V3" gate="G$1" x="53.34" y="157.48"/>
<instance part="C5" gate="G$1" x="170.18" y="88.9" rot="R90"/>
<instance part="Q4" gate="G$1" x="149.86" y="99.06"/>
<instance part="Q6" gate="G$1" x="160.02" y="106.68"/>
<instance part="R9" gate="G$1" x="152.4" y="114.3" rot="R90"/>
<instance part="+12V7" gate="G$1" x="152.4" y="121.92"/>
<instance part="+12V9" gate="G$1" x="162.56" y="116.84"/>
<instance part="Q7" gate="G$1" x="160.02" y="66.04"/>
<instance part="Q5" gate="G$1" x="154.94" y="58.42" rot="MR0"/>
<instance part="R10" gate="G$1" x="152.4" y="73.66" rot="R90"/>
<instance part="+12V8" gate="G$1" x="152.4" y="81.28"/>
<instance part="GND10" gate="1" x="152.4" y="48.26"/>
<instance part="R11" gate="G$1" x="162.56" y="50.8" rot="R90"/>
<instance part="GND11" gate="1" x="162.56" y="43.18"/>
<instance part="R8" gate="G$1" x="134.62" y="88.9"/>
<instance part="C4" gate="G$1" x="135.89" y="81.28" rot="R270"/>
<instance part="R12" gate="G$1" x="180.34" y="81.28" rot="R90"/>
<instance part="GND12" gate="1" x="180.34" y="73.66"/>
<instance part="CN2" gate="G$1" x="63.5" y="76.2" rot="MR0"/>
<instance part="CN3" gate="G$1" x="193.04" y="88.9"/>
<instance part="CN1" gate="G$1" x="25.4" y="149.86" rot="MR0"/>
<instance part="+12V1" gate="G$1" x="33.02" y="157.48"/>
<instance part="GND1" gate="1" x="33.02" y="139.7"/>
<instance part="LED1" gate="G$1" x="66.04" y="144.78"/>
<instance part="R1" gate="G$1" x="66.04" y="152.4" rot="R90"/>
<instance part="+12V4" gate="G$1" x="66.04" y="160.02"/>
<instance part="GND4" gate="1" x="66.04" y="137.16"/>
<instance part="GND5" gate="1" x="68.58" y="66.04"/>
<instance part="GND13" gate="1" x="187.96" y="78.74"/>
</instances>
<busses>
</busses>
<nets>
<net name="N$1" class="0">
<segment>
<pinref part="R5" gate="G$1" pin="1"/>
<pinref part="R6" gate="G$1" pin="2"/>
</segment>
</net>
<net name="GND" class="0">
<segment>
<pinref part="R2" gate="G$1" pin="1"/>
<pinref part="GND6" gate="1" pin="GND"/>
</segment>
<segment>
<pinref part="R6" gate="G$1" pin="1"/>
<pinref part="GND8" gate="1" pin="GND"/>
</segment>
<segment>
<pinref part="GND9" gate="1" pin="GND"/>
<pinref part="Q3" gate="G$1" pin="E"/>
<wire x1="116.84" y1="68.58" x2="116.84" y2="71.12" width="0.1524" layer="91"/>
</segment>
<segment>
<pinref part="R4" gate="G$1" pin="1"/>
<pinref part="GND7" gate="1" pin="GND"/>
</segment>
<segment>
<pinref part="GND2" gate="1" pin="GND"/>
<pinref part="C1" gate="G$1" pin="2"/>
<wire x1="43.18" y1="142.24" x2="43.18" y2="144.78" width="0.1524" layer="91"/>
</segment>
<segment>
<pinref part="GND3" gate="1" pin="GND"/>
<pinref part="C2" gate="G$1" pin="-"/>
<wire x1="53.34" y1="142.24" x2="53.34" y2="144.78" width="0.1524" layer="91"/>
</segment>
<segment>
<pinref part="R11" gate="G$1" pin="1"/>
<pinref part="GND11" gate="1" pin="GND"/>
</segment>
<segment>
<pinref part="GND10" gate="1" pin="GND"/>
<pinref part="Q5" gate="G$1" pin="E"/>
<wire x1="152.4" y1="50.8" x2="152.4" y2="53.34" width="0.1524" layer="91"/>
</segment>
<segment>
<pinref part="R12" gate="G$1" pin="1"/>
<pinref part="GND12" gate="1" pin="GND"/>
</segment>
<segment>
<pinref part="CN1" gate="G$1" pin="2"/>
<pinref part="GND1" gate="1" pin="GND"/>
<wire x1="25.4" y1="147.32" x2="33.02" y2="147.32" width="0.1524" layer="91"/>
<wire x1="33.02" y1="147.32" x2="33.02" y2="142.24" width="0.1524" layer="91"/>
</segment>
<segment>
<pinref part="LED1" gate="G$1" pin="C"/>
<pinref part="GND4" gate="1" pin="GND"/>
</segment>
<segment>
<pinref part="CN2" gate="G$1" pin="2"/>
<pinref part="GND5" gate="1" pin="GND"/>
<wire x1="63.5" y1="73.66" x2="68.58" y2="73.66" width="0.1524" layer="91"/>
<wire x1="68.58" y1="73.66" x2="68.58" y2="68.58" width="0.1524" layer="91"/>
</segment>
<segment>
<pinref part="CN3" gate="G$1" pin="2"/>
<pinref part="GND13" gate="1" pin="GND"/>
<wire x1="193.04" y1="86.36" x2="187.96" y2="86.36" width="0.1524" layer="91"/>
<wire x1="187.96" y1="86.36" x2="187.96" y2="81.28" width="0.1524" layer="91"/>
</segment>
</net>
<net name="INPUT" class="0">
<segment>
<pinref part="R2" gate="G$1" pin="2"/>
<wire x1="76.2" y1="73.66" x2="76.2" y2="76.2" width="0.1524" layer="91"/>
<pinref part="C3" gate="G$1" pin="-"/>
<wire x1="76.2" y1="76.2" x2="81.28" y2="76.2" width="0.1524" layer="91"/>
<junction x="76.2" y="76.2"/>
<pinref part="CN2" gate="G$1" pin="1"/>
<wire x1="63.5" y1="76.2" x2="76.2" y2="76.2" width="0.1524" layer="91"/>
</segment>
</net>
<net name="N$2" class="0">
<segment>
<pinref part="C3" gate="G$1" pin="+"/>
<pinref part="R3" gate="G$1" pin="1"/>
<wire x1="88.9" y1="76.2" x2="91.44" y2="76.2" width="0.1524" layer="91"/>
</segment>
</net>
<net name="+3V3" class="0">
<segment>
<pinref part="R7" gate="G$1" pin="2"/>
<pinref part="+12V6" gate="G$1" pin="+3V3"/>
</segment>
<segment>
<pinref part="Q1" gate="G$1" pin="E"/>
<pinref part="+12V5" gate="G$1" pin="+3V3"/>
</segment>
<segment>
<pinref part="C1" gate="G$1" pin="1"/>
<pinref part="+12V2" gate="G$1" pin="+3V3"/>
<wire x1="43.18" y1="152.4" x2="43.18" y2="154.94" width="0.1524" layer="91"/>
</segment>
<segment>
<pinref part="C2" gate="G$1" pin="+"/>
<pinref part="+12V3" gate="G$1" pin="+3V3"/>
<wire x1="53.34" y1="152.4" x2="53.34" y2="154.94" width="0.1524" layer="91"/>
</segment>
<segment>
<pinref part="R9" gate="G$1" pin="2"/>
<pinref part="+12V7" gate="G$1" pin="+3V3"/>
</segment>
<segment>
<pinref part="Q6" gate="G$1" pin="E"/>
<pinref part="+12V9" gate="G$1" pin="+3V3"/>
<wire x1="162.56" y1="114.3" x2="162.56" y2="111.76" width="0.1524" layer="91"/>
</segment>
<segment>
<pinref part="R10" gate="G$1" pin="2"/>
<pinref part="+12V8" gate="G$1" pin="+3V3"/>
</segment>
<segment>
<pinref part="CN1" gate="G$1" pin="1"/>
<pinref part="+12V1" gate="G$1" pin="+3V3"/>
<wire x1="25.4" y1="149.86" x2="33.02" y2="149.86" width="0.1524" layer="91"/>
<wire x1="33.02" y1="149.86" x2="33.02" y2="154.94" width="0.1524" layer="91"/>
</segment>
<segment>
<pinref part="R1" gate="G$1" pin="2"/>
<pinref part="+12V4" gate="G$1" pin="+3V3"/>
</segment>
</net>
<net name="N$4" class="0">
<segment>
<pinref part="R4" gate="G$1" pin="2"/>
<pinref part="Q1" gate="G$1" pin="C"/>
<wire x1="106.68" y1="109.22" x2="106.68" y2="111.76" width="0.1524" layer="91"/>
<pinref part="Q2" gate="G$1" pin="B"/>
<wire x1="106.68" y1="111.76" x2="106.68" y2="114.3" width="0.1524" layer="91"/>
<wire x1="111.76" y1="111.76" x2="106.68" y2="111.76" width="0.1524" layer="91"/>
<junction x="106.68" y="111.76"/>
</segment>
</net>
<net name="N$5" class="0">
<segment>
<pinref part="Q2" gate="G$1" pin="E"/>
<pinref part="R7" gate="G$1" pin="1"/>
<wire x1="116.84" y1="116.84" x2="116.84" y2="119.38" width="0.1524" layer="91"/>
<pinref part="Q1" gate="G$1" pin="B"/>
<wire x1="116.84" y1="119.38" x2="116.84" y2="121.92" width="0.1524" layer="91"/>
<wire x1="111.76" y1="119.38" x2="116.84" y2="119.38" width="0.1524" layer="91"/>
<junction x="116.84" y="119.38"/>
</segment>
</net>
<net name="N$6" class="0">
<segment>
<pinref part="R9" gate="G$1" pin="1"/>
<wire x1="152.4" y1="109.22" x2="152.4" y2="106.68" width="0.1524" layer="91"/>
<pinref part="Q4" gate="G$1" pin="C"/>
<pinref part="Q6" gate="G$1" pin="B"/>
<wire x1="152.4" y1="106.68" x2="152.4" y2="104.14" width="0.1524" layer="91"/>
<wire x1="157.48" y1="106.68" x2="152.4" y2="106.68" width="0.1524" layer="91"/>
<junction x="152.4" y="106.68"/>
</segment>
</net>
<net name="N$8" class="0">
<segment>
<pinref part="Q5" gate="G$1" pin="C"/>
<pinref part="R10" gate="G$1" pin="1"/>
<wire x1="152.4" y1="63.5" x2="152.4" y2="66.04" width="0.1524" layer="91"/>
<pinref part="Q7" gate="G$1" pin="B"/>
<wire x1="152.4" y1="66.04" x2="152.4" y2="68.58" width="0.1524" layer="91"/>
<wire x1="157.48" y1="66.04" x2="152.4" y2="66.04" width="0.1524" layer="91"/>
<junction x="152.4" y="66.04"/>
</segment>
</net>
<net name="N$9" class="0">
<segment>
<pinref part="R11" gate="G$1" pin="2"/>
<pinref part="Q7" gate="G$1" pin="E"/>
<wire x1="162.56" y1="55.88" x2="162.56" y2="58.42" width="0.1524" layer="91"/>
<pinref part="Q5" gate="G$1" pin="B"/>
<wire x1="162.56" y1="58.42" x2="162.56" y2="60.96" width="0.1524" layer="91"/>
<wire x1="157.48" y1="58.42" x2="162.56" y2="58.42" width="0.1524" layer="91"/>
<junction x="162.56" y="58.42"/>
</segment>
</net>
<net name="DCOUTPUT" class="0">
<segment>
<pinref part="Q4" gate="G$1" pin="E"/>
<pinref part="R8" gate="G$1" pin="2"/>
<wire x1="139.7" y1="88.9" x2="142.24" y2="88.9" width="0.1524" layer="91"/>
<wire x1="142.24" y1="88.9" x2="142.24" y2="81.28" width="0.1524" layer="91"/>
<pinref part="C4" gate="G$1" pin="1"/>
<wire x1="142.24" y1="81.28" x2="138.43" y2="81.28" width="0.1524" layer="91"/>
<pinref part="Q6" gate="G$1" pin="C"/>
<wire x1="162.56" y1="88.9" x2="162.56" y2="101.6" width="0.1524" layer="91"/>
<wire x1="142.24" y1="88.9" x2="152.4" y2="88.9" width="0.1524" layer="91"/>
<wire x1="152.4" y1="88.9" x2="162.56" y2="88.9" width="0.1524" layer="91"/>
<wire x1="152.4" y1="93.98" x2="152.4" y2="88.9" width="0.1524" layer="91"/>
<pinref part="Q7" gate="G$1" pin="C"/>
<wire x1="162.56" y1="71.12" x2="162.56" y2="88.9" width="0.1524" layer="91"/>
<junction x="152.4" y="88.9"/>
<junction x="162.56" y="88.9"/>
<pinref part="C5" gate="G$1" pin="+"/>
<wire x1="162.56" y1="88.9" x2="167.64" y2="88.9" width="0.1524" layer="91"/>
<junction x="142.24" y="88.9"/>
</segment>
</net>
<net name="INTSTAGE" class="0">
<segment>
<pinref part="Q2" gate="G$1" pin="C"/>
<pinref part="Q3" gate="G$1" pin="C"/>
<wire x1="116.84" y1="106.68" x2="116.84" y2="99.06" width="0.1524" layer="91"/>
<pinref part="Q4" gate="G$1" pin="B"/>
<wire x1="116.84" y1="99.06" x2="116.84" y2="81.28" width="0.1524" layer="91"/>
<wire x1="147.32" y1="99.06" x2="116.84" y2="99.06" width="0.1524" layer="91"/>
<junction x="116.84" y="99.06"/>
</segment>
</net>
<net name="NFB" class="0">
<segment>
<pinref part="R8" gate="G$1" pin="1"/>
<wire x1="129.54" y1="88.9" x2="127" y2="88.9" width="0.1524" layer="91"/>
<wire x1="127" y1="88.9" x2="127" y2="81.28" width="0.1524" layer="91"/>
<pinref part="C4" gate="G$1" pin="2"/>
<wire x1="127" y1="81.28" x2="130.81" y2="81.28" width="0.1524" layer="91"/>
<pinref part="R3" gate="G$1" pin="2"/>
<pinref part="Q3" gate="G$1" pin="B"/>
<wire x1="101.6" y1="76.2" x2="106.68" y2="76.2" width="0.1524" layer="91"/>
<pinref part="R5" gate="G$1" pin="2"/>
<wire x1="106.68" y1="76.2" x2="111.76" y2="76.2" width="0.1524" layer="91"/>
<wire x1="106.68" y1="73.66" x2="106.68" y2="76.2" width="0.1524" layer="91"/>
<junction x="106.68" y="76.2"/>
<wire x1="127" y1="88.9" x2="106.68" y2="88.9" width="0.1524" layer="91"/>
<wire x1="106.68" y1="88.9" x2="106.68" y2="76.2" width="0.1524" layer="91"/>
<junction x="127" y="88.9"/>
</segment>
</net>
<net name="OUTPUT" class="0">
<segment>
<pinref part="C5" gate="G$1" pin="-"/>
<pinref part="R12" gate="G$1" pin="2"/>
<wire x1="175.26" y1="88.9" x2="180.34" y2="88.9" width="0.1524" layer="91"/>
<wire x1="180.34" y1="88.9" x2="180.34" y2="86.36" width="0.1524" layer="91"/>
<junction x="180.34" y="88.9"/>
<pinref part="CN3" gate="G$1" pin="1"/>
<wire x1="193.04" y1="88.9" x2="180.34" y2="88.9" width="0.1524" layer="91"/>
</segment>
</net>
<net name="N$3" class="0">
<segment>
<pinref part="LED1" gate="G$1" pin="A"/>
<pinref part="R1" gate="G$1" pin="1"/>
</segment>
</net>
</nets>
</sheet>
</sheets>
<errors>
<approved hash="113,1,130.122,89.4818,FRAME1,,,,,"/>
</errors>
</schematic>
</drawing>
</eagle>
