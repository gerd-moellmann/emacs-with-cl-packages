;; -*- mode: lisp-data; -*-
((pass 1 "PASS :changeme"))
((nick 1 "NICK tester"))
((user 1 "USER user 0 * :tester")
 (0 ":irc.foonet.org 001 tester :Welcome to the foonet IRC Network tester")
 (0 ":irc.foonet.org 002 tester :Your host is irc.foonet.org, running version oragono-2.6.0-7481bf0385b95b16")
 (0 ":irc.foonet.org 003 tester :This server was created Sun, 13 Jun 2021 05:45:20 UTC")
 (0 ":irc.foonet.org 004 tester irc.foonet.org oragono-2.6.0-7481bf0385b95b16 BERTZios CEIMRUabefhiklmnoqstuv Iabefhkloqv")
 (0 ":irc.foonet.org 005 tester AWAYLEN=390 BOT=B CASEMAPPING=ascii CHANLIMIT=#:100 CHANMODES=Ibe,k,fl,CEMRUimnstu CHANNELLEN=64 CHANTYPES=# ELIST=U EXCEPTS EXTBAN=,m FORWARD=f INVEX KICKLEN=390 :are supported by this server")
 (0 ":irc.foonet.org 005 tester MAXLIST=beI:60 MAXTARGETS=4 MODES MONITOR=100 NETWORK=foonet NICKLEN=32 PREFIX=(qaohv)~&@%+ STATUSMSG=~&@%+ TARGMAX=NAMES:1,LIST:1,KICK:1,WHOIS:1,USERHOST:10,PRIVMSG:4,TAGMSG:4,NOTICE:4,MONITOR:100 TOPICLEN=390 UTF8MAPPING=rfc8265 UTF8ONLY WHOX :are supported by this server")
 (0 ":irc.foonet.org 005 tester draft/CHATHISTORY=100 :are supported by this server")
 (0 ":irc.foonet.org 251 tester :There are 0 users and 4 invisible on 1 server(s)")
 (0 ":irc.foonet.org 252 tester 0 :IRC Operators online")
 (0 ":irc.foonet.org 253 tester 0 :unregistered connections")
 (0 ":irc.foonet.org 254 tester 1 :channels formed")
 (0 ":irc.foonet.org 255 tester :I have 4 clients and 0 servers")
 (0 ":irc.foonet.org 265 tester 4 4 :Current local users 4, max 4")
 (0 ":irc.foonet.org 266 tester 4 4 :Current global users 4, max 4")
 (0 ":irc.foonet.org 422 tester :MOTD File is missing"))

((mode-user 10 "MODE tester +i")
 (0 ":irc.foonet.org 221 tester +i")
 (0 ":irc.foonet.org NOTICE tester :This server is in debug mode and is logging all user I/O. If you do not wish for everything you send to be readable by the server owner(s), please disconnect."))

((join 15 "JOIN #chan")
 (0 ":tester!~u@yuvqisyu7m7qs.irc JOIN #chan")
 (0 ":irc.foonet.org 353 tester = #chan :tester @alice bob")
 (0 ":irc.foonet.org 366 tester #chan :End of NAMES list"))

((mode 10 "MODE #chan")
 (0.0 ":irc.foonet.org 324 tester #chan +nt")
 (0.0 ":irc.foonet.org 329 tester #chan 1623563121")
 (0.0 ":alice!~u@wyb9b355rgzi8.irc PRIVMSG #chan :tester, welcome!")
 (0.0 ":bob!~u@wyb9b355rgzi8.irc PRIVMSG #chan :tester, welcome!")
 (0.1 ":bob!~u@wyb9b355rgzi8.irc PRIVMSG #chan :alice: Marry, that, I think, be young Petruchio.")
 (0.4 ":alice!~u@wyb9b355rgzi8.irc PRIVMSG #chan :bob: You speak of him when he was less furnished than now he is with that which makes him both without and within.")
 (0.2 ":chester!~u@yuvqisyu7m7qs.irc JOIN #chan")
 (0.1 ":alice!~u@wyb9b355rgzi8.irc PRIVMSG #chan :chester, welcome!")
 (0.1 ":bob!~u@wyb9b355rgzi8.irc PRIVMSG #chan :chester, welcome!"))

((nick 5 "NICK dummy")
 (0 ":tester!~u@gq7yjr7gsu7nn.irc NICK :dummy")
 (0.1 ":dummy!~u@gq7yjr7gsu7nn.irc MODE dummy :+RZi")
 (0.1 ":bob!~u@wyb9b355rgzi8.irc PRIVMSG #chan :alice: That ever eye with sight made heart lament.")
 (0.1 ":alice!~u@wyb9b355rgzi8.irc PRIVMSG #chan :bob: The bitter past, more welcome is the sweet.")
 (0.1 ":bob!~u@wyb9b355rgzi8.irc PRIVMSG #chan :alice: Dispatch, I say, and find the forester."))
