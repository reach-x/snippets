	<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
	<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
	<title>Payment Discounts | Exclusive Payment Discounts</title>
	<link rel="stylesheet" type="text/css" media="all" href="CSS/layout.css"/>
	<link rel="stylesheet" type="text/css" media="all" href="CSS/navigation.css"/>
	<link rel="stylesheet" type="text/css" media="all" href="CSS/popup.css"/>
	<script type="text/javascript" language="javascript" src="http://ajax.googleapis.com/ajax/libs/jquery/1.4/jquery.min.js"></script>
	<script type="text/javascript" language="javascript" src="JQuery/jquery.register.js"></script>
	<script type="text/javascript" language="javascript" src="JQuery/jquery.innerfade.js"></script>
	<script type="text/javascript" src="JQuery/jquery.popup.js.php?full=1"></script>
	<script type="text/javascript" language="javascript" src="Scripts/functions.js"></script>
	<script type="text/javascript">
		hs.graphicsDir = 'JQuery/graphics/';
		hs.outlineType = 'rounded-white';
		hs.wrapperClassName = 'draggable-header';
		hs.fadeInOut = true;
		hs.dimmingOpacity = 0.50;
		$(document).ready(function () {
			$('#tips').innerfade({
				speed: 2000,
				timeout: 8000,
			});
		});
	</script>
</head>
<body>
	<div id="headerContainer" class="container" align="center">
		<div id="navigationHolder">
			<div id="navigation">
				<div id="nav1" class="float" style="height:15px; width:129px; background:url(Images/menu/nav1.jpg) no-repeat; background-position: 0 -13px;"></div>
				<div id="nav2" class="float"><a href="/News" class="srollover"></a></div>
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
		<div id="header" align="right"></div>
	</div>

	<div id="mainContainer" class="container" align="center">
		<div id="main" align="left">
			<div id="leftColumn">
				<div id="formRegistration">
					<div id="register" align="left">
						<form method="post" action="Scripts/registration_process.php" name="registerform" id="registerform">
							<fieldset>
								<input name="firstName" type="text" id="firstName" size="30" value="" tabindex="1" style="margin-bottom:27px;"/>
								<br/>
								<input name="lastName" type="text" id="lastName" size="30" value="" tabindex="2" style="margin-bottom:25px;"/>
								<br/>
								<input name="email" type="text" id="email" size="30" value="" tabindex="3" style="margin-bottom:27px;"/>
								<br/>
								<!--&nbsp;<input name="optIn" type="checkbox" id="optIn"  checked="checked"  tabindex="4" />--> &nbsp;<input name="optIn" type="checkbox" id="optIn" tabindex="4"/>
								<div style="padding-top:50px;">
									<input type="image" class="submit" id="submit" style="width:183px; height:33px; border:none;" src="Images/register_button.jpg"/>
								</div>
							</fieldset>
						</form>
					</div>
					<div align="left" style="height:20px; padding-left:8px;">
						<div id="register_preloader"></div>
						<div id="register_message" style="text-align:center;"></div>
					</div>
				</div>
			</div>
			<div id="rightColumn">
				<div id="topRightColumn" style="height:135px;">
					<div class="float" style="width:132px; padding-left:34px; padding-top:10px;">
						<ul id="tips">
							<li>
								<img src="Images/thumbnails/1.gif" border="0" alt="Payment Discounts"/>
							</li>
							<li>
								<img src="Images/thumbnails/2.gif" border="0" alt="Payment Discounts"/>
							</li>
							<li>
								<img src="Images/thumbnails/3.gif" border="0" alt="Payment Discounts"/>
							</li>
						</ul>
					</div>
					<div class="float" style="width:359px; text-align:justify; padding-top:6px;">Welcome to Payment Disount. Our network of affiliates has allowed us to offer you the biggest savings for all your monthly payments in one centralized location. Register today to receive discount offers for Auto, Health, Life and Home Inusrance as well as great savings on Grants, Product offers, and exclusive deals!</div>
					<div style="clear:left;"></div>
				</div>
				<div id="bottomRightColumn">
					<div class="float" style="width:312px; padding-left:34px; padding-top:20px">
						<p>
							<img src="Images/thumbnails/auto.jpg" width="116" height="81" border="0" onmouseover="this.style.opacity=0.4;this.filters.alpha.opacity=40" onmouseout="this.style.opacity=1;this.filters.alpha.opacity=100"/>
						</p>
						<p>
							<img src="Images/thumbnails/health.jpg" width="116" height="81" border="0" onmouseover="this.style.opacity=0.4;this.filters.alpha.opacity=40" onmouseout="this.style.opacity=1;this.filters.alpha.opacity=100"/>
						</p>
					</div>
					<div class="float" style="width:165px; padding-top:25px; padding-left:18px;">
						<div id="side_articles" style="font-size:10px; text-align:justify;">
							<?php
							require_once("Scripts/commonlib.php");
							$url = "http://www.feedzilla.com/rss/products/top-stories";
							//url, number of articles, channel on/off, description on/off, date on/off
							//echo Common_Display($url, 2, false, false, false);
							?>
						</div>
					</div>
					<div style="clear:left;"></div>
				</div>
			</div>
			<div style="clear:left;"></div>
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
