<?php

$headers = <<<EOH
Delivered-To: topica@reach-x.com
Received: by 10.64.245.232 with SMTP id xr8csp1576553iec; Tue, 28 Feb 2017
 12:51:11 -0800 (PST)
X-Received: by 10.157.54.139 with SMTP id h11mr2526463otc.86.1488315071316;
 Tue, 28 Feb 2017 12:51:11 -0800 (PST)
Return-Path: <<username>>
Received: from mail-oi0-x22c.google.com (mail-oi0-x22c.google.com.
 [2607:f8b0:4003:c06::22c]) by mx.google.com with ESMTPS id
 d206si1224371oif.297.2017.02.28.12.51.11 for <topica@reach-x.com>
 (version=TLS1_2 cipher=ECDHE-RSA-AES128-GCM-SHA256 bits=128/128); Tue, 28 Feb
 2017 12:51:11 -0800 (PST)
Received-SPF: pass (google.com: domain of <username> designates
 2607:f8b0:4003:c06::22c as permitted sender)
 client-ip=2607:f8b0:4003:c06::22c;
Authentication-Results: mx.google.com; dkim=pass header.i=@popularllc.com;
 spf=pass (google.com: domain of <username> designates
 2607:f8b0:4003:c06::22c as permitted sender) smtp.mailfrom=<username>
Received: by mail-oi0-x22c.google.com with SMTP id f192so12261877oic.3 for
 <topica@reach-x.com>; Tue, 28 Feb 2017 12:51:11 -0800 (PST)
DKIM-Signature: v=1; a=rsa-sha256; c=relaxed/relaxed; d=popularllc.com;
 s=google;
 h=mime-version:in-reply-to:references:from:date:message-id:subject:to;
 bh=lSLyvWds/iSoCZidPs/3RbiI0gF4p7AARdvnlWb23vI=;
 b=aVwWxH4tduoKM3A7G0eQ+xzPNmI/4mhroL1rVbulGVWR/pRinZnpDw1fibhVxXq7rj
 gPXWRPaNhv1qaAB44CrBeFCN2OOQuQNbU0NS4oGDzxfPd+ZlJvjKGJ8PaDIpDdbRdk6A
 Solql+AtMhJrPhHR6zMdt6YbnTyCZIGGu0TZU=
X-Google-DKIM-Signature: v=1; a=rsa-sha256; c=relaxed/relaxed; d=1e100.net;
 s=20161025;
 h=x-gm-message-state:mime-version:in-reply-to:references:from:date
 :message-id:subject:to; bh=lSLyvWds/iSoCZidPs/3RbiI0gF4p7AARdvnlWb23vI=;
 b=ol5po41cT+HAe02XIzRci4jU1Ift0EzkQIiVxLCwMKKZkcO9ENOblUoYvi9fMU5gIb
 yDykgDHNJ3mAFZswOxaViqKvJbTvxCYbUqCS37C9ZPN+CZx8TN+bP7p+oSwfZZRLDGfo
 ZfU82c1Iom+rxhrYbaqWuJHrDFrv5cIQ7HQl1JExh2YaCyKCFDHf/amWxNdtqS7HFBU1
 GahmhN6C9vFWcBzivTcG3TqV3WxqjSMpGi7EW1aZhpJIBOEKvnapR5A9N0+qUtiv1tGu
 NVdGDNoB5Gszbcmo3cywW1daA93C+hKFpSMu8P+GTVzAD0IkMuEZPF/7EZjUMdAOC1V2 4oUQ==
X-Gm-Message-State: AMke39ndi0AD4zGIYHhAmJn2W2RkGd0P/jMw5/UbMcZRRkCWnJVAzpy9V2a4NXjCLpPlJXBupeHgGAs1U7FLDGG2xDY=
X-Received: by 10.202.50.7 with SMTP id y7mr2330245oiy.111.1488315070702; Tue,
 28 Feb 2017 12:51:10 -0800 (PST)
MIME-Version: 1.0
Received: by 10.74.19.88 with HTTP; Tue, 28 Feb 2017 12:50:50 -0800 (PST)
In-Reply-To: <0.0.15.A5.1D291FDAD7E7762.0@tnews-ccxx.tailorednews.com>
References: <0.0.15.A5.1D291FDAD7E7762.0@tnews-ccxx.tailorednews.com>
From: John Brahy <jb@popularllc.com>
Date: Tue, 28 Feb 2017 12:50:50 -0800
Message-ID: <CA+Ko5+AikwfMH_PCiJkoOB7z7uVyhLujMW4+fOoNvU_JCD8u4A@mail.gmail.com>
Subject: Fwd: (Re)introducing your exclusive search
To: topica@reach-x.com
Content-Type: multipart/alternative; boundary=001a113caa3c1a7e3305499d5bcf
EOH;


print_r(preg_match_all("/^Received: by/im", $headers, $matched));

foreach (explode("\n", $headers) as $header) {

	if (preg_match("/Received/", $header)) {
		printf("%s\n", $header);
	}
}