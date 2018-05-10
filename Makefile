
all : add commit fetch pull push status

status : /usr/bin/git
	git status

commit : /usr/bin/git
	git commit -a

fetch : /usr/bin/git
	git fetch --all

pull : /usr/bin/git
	git pull --all

push : /usr/bin/git
	git push --all

add : /usr/bin/git
	git add .