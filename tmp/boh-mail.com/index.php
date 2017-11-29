<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
<title>Biz Offer Hub| The Hub of Exclusive Offers</title>
<link rel="stylesheet" type="text/css" media="all" href="../CSS/layout.css" />
<link rel="stylesheet" type="text/css" media="all" href="../CSS/popup.css" />
<script type="text/javascript" language="javascript" src="http://ajax.googleapis.com/ajax/libs/jquery/1.4/jquery.min.js"></script>
<script type="text/javascript" language="javascript" src="../JQuery/jquery.register.js"></script>
<script type="text/javascript" language="javascript" src="../JQuery/jquery.newsletter.js"></script>
<script type="text/javascript" language="javascript" src="../JQuery/jquery.innerfade.js"></script>
<script type="text/javascript" src="../JQuery/jquery.popup.js.php?full=1"></script>
<script type="text/javascript" language="javascript" src="../JS/functions1.js"></script>
<script type="text/javascript">
hs.graphicsDir = '../JQuery/graphics/';
hs.outlineType = 'rounded-white';
hs.wrapperClassName = 'draggable-header';
hs.fadeInOut = true;
hs.dimmingOpacity = 0.50;
$(document).ready(
		function(){
			$('#tips').innerfade({
				speed: 2000,
				timeout: 8000,
			});
	});
</script>
</head>
<body>
<div id="headerContainer" class="container" align="center">
	<div id="header" align="center">
    	<div id="navigation" align="center" style="padding-top:18px;"><a href="/">Home</a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span style="color:#FF9900; font-weight:bold; font-size:13px;">News</span>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="/Unsubscribe" onclick="return hs.htmlExpand(this, { objectType: 'iframe'} )">Unsubscribe</a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="/Policy" onclick="return hs.htmlExpand(this, { objectType: 'ajax' , width:800} )">Policy</a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="/Contact" onclick="return hs.htmlExpand(this, { objectType: 'iframe'} )">Contact</a></div>
    </div>
</div>
<div id="mainContainer" class="container" align="center">
    	<div id="main_news" align="left">
        	<div id="leftColumn">
				<div id="formRegistration">
                	<div id="register" align="left">
                		<form method="post" action="../Scripts/registration_process.php" name="registerform" id="registerform">
                		<fieldset>
                        <input name="firstName" type="text" id="firstName" size="30" value="" tabindex="1" style="margin-bottom:27px;" />
                        <br />
                        <input name="lastName" type="text" id="lastName" size="30" value=""  tabindex="2" style="margin-bottom:25px;" />
                        <br />
                        <input name="email" type="text" id="email" size="30" value=""  tabindex="3" style="margin-bottom:27px;" />
						<br />
						&nbsp;<input name="optIn" type="checkbox" id="optIn"  checked="checked"  tabindex="4" />
						<div style="padding-top:40px;">
                		<input type="image" class="submit" id="submit" style="width:183px; height:33px; border:none;" src="../Images/register_button.jpg" />
                        </div>
                		</fieldset>
               			</form>
        			</div>
            		<div align="left" style="height:20px; padding-left:8px;">
                        <div id="register_preloader"></div>
                        <div id="register_message" style="text-align:center;"></div>
                    </div>
				</div>
            <!--<div id="newsletter" align="left">
                			<form method="post" action="../Scripts/newsletter_process.php" name="newsletterform" id="newsletterform">
                			<fieldset>
                            <div style="padding-left:25px;">
                            <input name="newsEmail" type="text" id="newsEmail" size="30" value="" />
                            </div>
                            <div align="center" style="height:44px; padding-top:5px;">
                			<input type="image" class="submit" id="newsSubmit" style="width:183px; height:33px;" src="../Images/news_button.jpg" />
                            </div>
                			</fieldset>
                            <div style="height:20px;">
                            <div align="center" id="newsletter_message"></div>
                            <div align="center" id="newsletter_preloader"></div>
                            </div>
               				</form>
        			   </div>-->
            </div>
            <div id="rightColumn">
            	<div id="topRightColumn" style="height:200px;">
                    	 <div id="side_articles" style="font-size:10px; padding-top:60px; padding-right:20px; text-align:justify;">
                		 <?php
//						 require_once("../Scripts/commonlib.php");
//						 $url = "http://www.feedzilla.com/rss/products/top-stories";
//						 //url, number of articles, channel on/off, description on/off, date on/off
//						 echo Common_Display($url, 4, false, true, false);
						 ?>
						 </div>
                </div>
            </div>
            <div style="clear:left;"></div>
        </div>
</div>
<div id="footerContainer" class="container" align="center">
    	<div id="footer">
        <div align="center" style="font-weight:bold; padding-top:30px;"><span style="color:#FFFFFF;"><a href="/">Home</a> | <a href="/News">News</a> | <a href="/Policy" onclick="return hs.htmlExpand(this, { objectType: 'ajax' , width: 800 } )">Policy</a> | <a href="/Unsubscribe" onclick="return hs.htmlExpand(this, { objectType: 'iframe' } )">Unsubscribe</a> | <a href="/Contact" onclick="return hs.htmlExpand(this, { objectType: 'iframe' } )">Contact</a></span><br /><span style="color:#FF9900;">Copyright 2010 Biz Offer Hub</span></div>
        </div>
</div>
</body>
</html>
