<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
	<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
	<title>Payment Discounts | Exclusive Payment Discounts</title>
	<link rel="stylesheet" type="text/css" media="all" href="../CSS/layout.css"/>
	<link rel="stylesheet" type="text/css" media="all" href="../CSS/navigation.css"/>
	<link rel="stylesheet" type="text/css" media="all" href="../CSS/popup.css"/>
	<script type="text/javascript" language="javascript" src="http://ajax.googleapis.com/ajax/libs/jquery/1.4/jquery.min.js"></script>
	<script type="text/javascript" language="javascript" src="../JQuery/jquery.newsletter.js"></script>
	<script type="text/javascript" src="../JQuery/jquery.popup.js.php?full=1"></script>
	<script type="text/javascript">
		hs.graphicsDir = '../JQuery/graphics/';
		hs.outlineType = 'rounded-white';
		hs.wrapperClassName = 'draggable-header';
		hs.fadeInOut = true;
		hs.dimmingOpacity = 0.50;
	</script>
</head>
<body>
	<div id="headerContainer" class="container" align="center">
		<div id="navigationHolder">
			<div id="navigation">
				<div id="nav1" class="float"><a href="/" class="srollover"></a></div>
				<div id="nav2" class="float" style="height:15px; width:138px; background:url(../Images/menu/nav2.jpg) no-repeat; background-position: 0 -13px;"></div>
				<div id="nav3" class="float">
					<a href="/Policy" class="srollover" onclick="return hs.htmlExpand(this, { objectType: 'ajax' , width: 800 } )"></a>
				</div>
				<div id="nav4" class="float">
					<a href="/Unsubscribe" class="srollover" onclick="return hs.htmlExpand(this, { objectType: 'iframe' } )"></a>
				</div>
				<div id="nav5" class="float">
					<a href="/Contact" class="srollover" onclick="return hs.htmlExpand(this, { objectType: 'iframe' } )"></a>
				</div>
				<div style="clear:left;"></div>
			</div>
		</div>
		<div id="header_news" align="right"></div>
	</div>

	<div id="mainContainer" class="container" align="center">
		<div id="main_news" align="left">
			<div id="leftColumn">
				<div id="formRegistration">
					<div id="register" align="left" style="padding-top:110px; height:200px;">
						<form method="post" action="../Scripts/newsletter_process.php" name="newsletterform" id="newsletterform">
							<fieldset>
								<input name="newsEmail" type="text" id="newsEmail" size="30" value="" tabindex="3" style="margin-bottom:27px;"/>
								<div style="padding-top:50px;">
									<input type="image" class="submit" id="newsSubmit" style="width:183px; height:33px; border:none;" src="../Images/news_button.jpg"/>
								</div>
							</fieldset>
						</form>
					</div>
					<div align="left" style="height:20px; padding-left:8px;">
						<div id="newsletter_preloader"></div>
						<div id="register_message" style="text-align:center;"></div>
					</div>
				</div>
			</div>
			<div id="rightColumn">
				<div id="topRightColumn" style="height:270px; padding-left:50px">
					<div id="side_articles" style="font-size:10px; text-align:left;">
						<?php
//						require_once("../Scripts/commonlib.php");
//						$url = "http://www.feedzilla.com/rss/products/top-stories";
//						//url, number of articles, channel on/off, description on/off, date on/off
//						echo Common_Display($url, 3, FALSE, TRUE, FALSE);
						?>
					</div>
				</div>
			</div>
		</div>
	</div>
	<div id="footerContainer" class="container" style="background-color:#333333;" align="center">
		<div id="footer">
			<div align="left" style="font-weight:bold; padding-left:300px;">
				<span style="color:#FFFFFF;"><a href="/">Home</a> | <a href="/News">News</a> | <a href="/Policy" onclick="return hs.htmlExpand(this, { objectType: 'ajax' , width: 800 } )">Policy</a> | <a href="/Unsubscribe" onclick="return hs.htmlExpand(this, { objectType: 'iframe' } )">Unsubscribe</a> | <a href="/Contact" onclick="return hs.htmlExpand(this, { objectType: 'iframe' } )">Contact</a></span><br/>Copyright 2010 Payment Discounts
			</div>
		</div>
	</div>
</body>
</html>
