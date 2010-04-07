%%%-------------------------------------------------------------------
%%% File    : lyricsfly.erl
%%% Author  : Jay<jdmundrawala@gmail.com>
%%% Description : Fetch lyrics from lyrics fly and encode them in json
%%%
%%% Created : 07 Aug 2009
%%%-------------------------------------------------------------------
-module(lyricsfly).
-include_lib("xmerl/include/xmerl.hrl").

-define(LF_KEY, "i=86182cbac27eac7c1-temporary.API.access").
-define(LF_API, "http://lyricsfly.com/api/api.php?").
-define(LF_SONG, "&a=~s&t=~s").
-define(LF_LYRIC, "&l=").

-record(song, {artist, title, album, lyrics}).
-export([init/0, search_by_song/2]).


init() ->
	ibrowse:start().

search_by_song(Artist, Song) ->
	Url = ?LF_API ++ ?LF_KEY ++ ?LF_SONG,
	Res = ibrowse:send_req(io_lib:format(Url,[ibrowse_lib:url_encode(Artist),
				ibrowse_lib:url_encode(Song)]),[], get),
	case Res of
		{ok, _S, _ResponseHeaders, ResponseBody} ->
			{XML, _} = xmerl_scan:string(ResponseBody),
			[Status] = xmerl_xpath:string("/start/status", XML),
			[StatusContent] = Status#xmlElement.content,
			if 
				StatusContent#xmlText.value == "300" ->
					SongList = xmerl_xpath:string("/start/sg", XML),
					[SongListHead|_SongListTail] = SongList,
					Sng = build_song(SongListHead#xmlElement.content, #song{}),
					{ok, Sng};
				StatusContent#xmlText.value /= "300" ->
					[]
			end
	end.

build_song([Head | Tail], Song) when Head#xmlElement.name == ar ->
	[XMLText] = Head#xmlElement.content,
	Text = XMLText#xmlText.value,
	build_song(Tail, Song#song{artist = Text});

build_song([Head | Tail], Song) when Head#xmlElement.name == tt ->
	[XMLText] = Head#xmlElement.content,
	Text = XMLText#xmlText.value,
	build_song(Tail, Song#song{title = Text});

build_song([Head | Tail], Song) when Head#xmlElement.name == al ->
	[XMLText] = Head#xmlElement.content,
	Text = XMLText#xmlText.value,
	build_song(Tail, Song#song{album = Text});

build_song([Head | Tail], Song) when Head#xmlElement.name == tx ->
	XML = Head#xmlElement.content,
	Text = compile_lyrics(XML),
	build_song(Tail, Song#song{lyrics = Text});
build_song([Head | Tail], Song) ->
	build_song(Tail, Song);
build_song([], Song) ->
	Song.

compile_lyrics(List) ->
	compile_lyrics(List, []).
compile_lyrics([Head| Tail], Str) when is_record(Head, xmlText) ->
	compile_lyrics(Tail, Str ++ Head#xmlText.value);
compile_lyrics([], Str) ->
	Str;
compile_lyrics([_Head | Tail], Str) ->
	compile_lyrics(Tail,Str).

