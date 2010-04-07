var chatCounter = 0;
//$(document).ready(function() {
//    current_items = getCurrentItems();
//});

$(".textFilter").click(function() {
    //$(this).background-color="#333333";
    alert("test");
});

function Client() {
    this.OldTypes=[];
    this.ActiveTypes=["Sites","Images","Twitter"];
    this.CurrentRoom="test_room";
    this.AllTypes=["TextFindings","Images","Twitter","Topics","Sites","Docs","Videos","YahooAnswers"];
    this.TotalUsers=0;    
    this.Topics=[];
    this.TotalUsers=0;
    Client.prototype.updateResults=function()
    {
        // Using ActiveTypes, create the text links in the system
    }
}

Client.prototype.ClearData=function() {
    for(var n=0;n<this.AllTypes.length;n=n+1) {
        $("#" + this.AllTypes[n]).find("div").each(function() {
            $(this).html("");
        });
    }
}

Client.prototype.onShowTopics=function(topics) {
    $("#chatyeo_search").jsonSuggest(topics,{onSelect:function(){}});
}

Client.prototype.Popup=function(type, row, rowClass, Id) {
    popupConversation(Id);
    // calls the modal popup with facebox
    jQuery.facebox({ ajax: '/web/modal_popup' });
    //  after the facebox has successfully loaded the page template, we insert the content we want in it.
    $(document).bind('reveal.facebox', function() {
        $("#contentDiv1").html($("#"+row +" ."+ rowClass).html());
        if (type == 'Image'){

            $("#contentDiv1 .profileimg").attr('style','cursor:pointer;');
            $("#contentDiv1 .profileimg").click(function(){
                var imgLnk = $("#contentDiv1 .profileimg").attr('src');
                window.open(imgLnk);
                return false;
            });
        }

    });
}
//Client.prototype.GetRealtimeSearchResults=function() {
 //   return(testData.searchChatyeo);
//}

Client.prototype.SetResultType=function(n, id) {
    if(this.ActiveTypes[n]!=id) {
        this.ActiveTypes[n]=id;
        //if(n<3) {
        //    subscribeSearch(this.ActiveTypes[n]);
        //}
        //else {
        //    unsubscribeSearch(this.ActiveTypes[n]);
        //}
    }
}

Client.prototype.setTotalUsers=function(users) {
    this.TotalUsers=users;
    $("#num_users").html(users);
}

Client.prototype.FindTypePosition=function(type) {
    for(var n=0;n<3;n=n+1)
    {
        if(this.ActiveTypes[n]==type)
        {
            return(n+1);
        }
    }
    return(-1);
}

Client.prototype.refreshCols=function() {
    for(var n=0;n<3;n=n+1)
    {
        if($("#" + this.ActiveTypes[n]).html()!=$("#col" +(n+1)).html()) {
            var newType=0;
            var tags=[];
            if(this.OldTypes[n]==this.ActiveTypes[n])
            {
                var m=0;
                $("#col" +(n+1)).children("div").each(function() {
                    tags[m]=$(this).html();
                    m=m+1;
                });
                newType=1;
            }
            $("#col" +(n+1)).html($("#" + this.ActiveTypes[n]).html());
            if(newType==1) {
                var m=0;
                $("#col" +(n+1)).find("div").each(function() {
                    if(tags[m]!=$(this).html()) {
                        var fade=17;
                        var fadeDiv=$(this);
                        var intervalId=setInterval(function() {
                            fadeDiv.css("background-color","rgb(" + (255-fade) +","+(255-fade)+","+(255-fade)+")");
                            fade-=1;
                            if(fade<=0) {
                                clearInterval(intervalId);
                            }
                        },40);
                    }
                    m=m+1;
                });
            }
        }
    }
    this.OldTypes=this.ActiveTypes.slice();
}
Client.prototype.onTextFindings=function(Id,result) {
var txt="";
    txt+="<div class=\"text-resultreturn\" onclick=\"client.Popup('TextFinding',this.parentNode.id,this.className,'" + Id + "');\">\n";
    txt+="<h3>" + result.Title + "</h3>\n";
    txt+="<p>" + result.Description + "</p>\n";
    txt+="</div>\n";
    var json = {};
    json["TextFindingsRow"+result.Row] = txt;

    if( $("#TextFindingsRow1").length > 0 ) {
      //  $("#TextFindings").fill(json);
    }
    else {
        var txt1="";
        txt1+="<div id=\"TextFindingsRow1\"><img src=\"/js/facebox/loading.gif\" /></div>\n";
        txt1+="<div id=\"TextFindingsRow2\"><img src=\"/js/facebox/loading.gif\" /></div>\n";
        txt1+="<div id=\"TextFindingsRow3\"><img src=\"/js/facebox/loading.gif\" /></div>\n";
        txt1+="<div id=\"TextFindingsRow4\"></div>\n";
        $("#TextFindings").html(txt1);
      //  $("#TextFindings").fill(json);
    }
}

Client.prototype.onImages=function(Id,result) {
var txt="";
    txt+="<div class=\"image-resultreturn\" onclick=\"client.Popup('Image',this.parentNode.id,this.className,'" + Id + "');\">";
    txt+="<img class=\"profileimg\" src=\"" + result.ImgUrl + "\"/>";
    txt+="<p>" + result.Title + "</p>";
    txt+="</div>";

    var json = {}
    json['ImagesRow'+result.Row] = txt;

    if( $("#ImagesRow1").length > 0 ) {
        $("#Images").fill(json);
    }
    else {
        var txt1="";
        txt1+="<div id=\"ImagesRow1\"><img src=\"/js/facebox/loading.gif\" /></div>\n";
        txt1+="<div id=\"ImagesRow2\"><img src=\"/js/facebox/loading.gif\" /></div>\n";
        txt1+="<div id=\"ImagesRow3\"><img src=\"/js/facebox/loading.gif\" /></div>\n";
        txt1+="<div id=\"ImagesRow4\"></div>\n";
        $("#Images").html(txt1);
        $("#Images").fill(json);
    }
}

Client.prototype.onBooks=function(Id,result) {
var txt="";
    txt+="<div onclick=\"client.Popup('Book',this.parentNode.id,this.className,'" + Id + "');\" class=\"book-resultreturn\">\n";
    txt+="<p class=\"title\">" + result.Title + "</p>";
    txt+="<p class=\"author\">" + result.Author + "</p>";
    txt+="<img src=\"" + result.ImgUrl + "\" class=\"bookcover\"/>";
    txt+="<p>" + result.Desc + "</p>";
    txt+="</div>";

    var json = {};
    json['BooksRow'+result.Row] = txt;

    if( $("#BooksRow1").length > 0 ) {
        $("#Books").fill(json);
    }
    else {
        var txt1="";
        txt1+="<div id=\"BooksRow1\"><img src=\"/js/facebox/loading.gif\" /></div>\n";
        txt1+="<div id=\"BooksRow2\"><img src=\"/js/facebox/loading.gif\" /></div>\n";
        txt1+="<div id=\"BooksRow3\"><img src=\"/js/facebox/loading.gif\" /></div>\n";
        txt1+="<div id=\"BooksRow4\"></div>\n";
        $("#Books").html(txt1);
        $("#Books").fill(json);
    }
}

Client.prototype.onWayFair=function(Id, json) {
    var txt="";
    //I don't know what goes here
    txt+="<div onclick=\"client.Popup('WayFair'," + json.Row + ",this.parentNode.id,this.className, '"+Id+"');\"" + "class=\"wayfair-resultreturn\">";
    txt+="<h3>" + json.Title +"</h3>\n";
    txt+="<img src=\"" + json.ImgUrl + "\"/>\n";
    //txt+="<h3>" + json[n].Subtitle + "</h3>\n";
    txt+="<p>" + json.Desc + "</p>\n";
    txt+="</div>";
    $("#WayFair #WayFairRow"+json.Row).html(txt);

}

Client.prototype.onTwitter=function(Id,result) {
var txt="";
    txt+="<div class=\"twitter-resultreturn\" onclick=\"client.Popup('Twitter',this.parentNode.id,this.className,'" + Id + "');\">";
    txt+="<p>" + result.User + "</p>";
    txt+="<img class=\"profileimg\" src=\"" + result.profileimg + "\"/>";
    txt+="<p>" + result.Question.replace(/http:\/\/\s+/g, '<a href="$&" target="_blank">$&</a>').replace(/\s(@)(\s+)/g, ' <a href="http://twitter.com/$2" target="_blank">$&</a>').replace(/\s(#)(\s+)/g, ' <a href="http://search.twitter.com/search?q=%23$2" target="_blank">$&</a>').replace(/^(\s+)/, '<b><a href=\"'+result.url+'\">$&</a></b>') + "</p>";
    txt+="<p>" + result.FromUser + "</p>";
    txt+="<p>" + result.Answer.replace(/http:\/\/\s+/g, '<a href="$&" target="_blank">$&</a>').replace(/\s(@)(\s+)/g, ' <a href="http://twitter.com/$2" target="_blank">$&</a>').replace(/\s(#)(\s+)/g, ' <a href="http://search.twitter.com/search?q=%23$2" target="_blank">$&</a>').replace(/^(\s+)/, '<b><a href=\"'+result.url+'\">$&</a></b>') + "</p>";
    txt+="<p>" + result.key + "</p>";
    txt+="<p>" + result.Time + "</p>";
    txt+="<p>" + result.Function + "</p>";
    txt+="<p>" + result.Depth + "</p>";
    txt+="<p>" + result.Url + "</p>";
    txt+="</div>";

    var json = {};
    json['TwitterRow'+result.Row] = txt;

    if( $("#TwitterRow1").length > 0 ) {
        $("#Twitter").fill(json);
    }
    else {
        var txt1="";
        txt1+="<div id=\"TwitterRow1\"><img src=\"/js/facebox/loading.gif\" /></div>\n";
        txt1+="<div id=\"TwitterRow2\"><img src=\"/js/facebox/loading.gif\" /></div>\n";
        txt1+="<div id=\"TwitterRow3\"><img src=\"/js/facebox/loading.gif\" /></div>\n";
        txt1+="<div id=\"TwitterRow4\"></div>\n";
        txt1+="<div id=\"TwitterRow5\"></div>\n";
        txt1+="<div id=\"TwitterRow6\"></div>\n";
        txt1+="<div id=\"TwitterRow7\"></div>\n";
        txt1+="<div id=\"TwitterRow8\"></div>\n";
        $("#Twitter").html(txt1);
        $("#Twitter").fill(json);
    }
}
/*
 * Function:     Client.onTopic(json)
 * Description:  Display admin topic information generated by bot_topic.
 * Parameters:
 * json = {
 *           "macro" : {
 *               "pt": ["s1", "s2", ...],
 *               "st": ["s1", "s2", ...],
 *            },
 *           "micro" : {
 *               "pt": ["s1", "s2", ...],
 *               "st": ["s1", "s2", ...],
 *            },
 *           "gs": ["s1", "s2", ...]
 *        }
 *
 *        where macro is the macro trend
 *              micro is the micro trend
 *              pt contains the array of strings displayed under the title "Primary Topics"
 *              st contains the array of strings displayed under the title "Secondary Topics"
 *              gs contains the array of strings displayed under the title "Google Suggest Topics"
 */
Client.prototype.onTopic=function(Id, json) {
  var txt="";
  txt+="<div style=\"max-height:150px;\">";
  txt+="<div id=\"TopicsDraggable\" class=\"topic-resultreturn\">";
  txt+="<h2>Macro Trend</h3>\n";
  txt+="<h3>Primary Topics</h3>\n";
  console.log(json);

  for(n in json.Data.macro.pt) {
    txt+="<p>" + json.Data.macro.pt[n] + "</p>";
  }

  txt+="<h3>Secondary Topics</h3>\n";
  for(n in json.Data.macro.st) {
    txt+="<p>" + json.Data.macro.st[n] + "</p>";
  }

  txt+="<h2>Micro Trend</h3>\n";
  txt+="<h3>Primary Topics</h3>\n";
  for(n in json.Data.micro.pt) {
    txt+="<p>" + json.Data.micro.pt[n] + "</p>";
  }
  txt+="<h3>Secondary Topics</h3>\n";
  for(n in json.Data.micro.st) {
    txt+="<p>" + json.Data.micro.st[n] + "</p>";
  }
  txt+="</div>";
  txt+="</div>";
$("#Topics").html(txt);
$("#TopicsDraggable").draggable();
}

Client.prototype.onSites=function(Id,result) {
    var txt="";
    txt+="<div onclick=\"client.Popup('Sites',this.parentNode.id,this.className,'" + Id + "');\" class=\"site-resultreturn\">";
    txt+="<p class=\"title\">" + result.Title + "</p>";
    txt+="<p>" + result.Message + "</p>";
    txt+="</div>";

    var json = {};
    json['SitesRow'+result.Row] = txt;

    if( $("#SitesRow1").length > 0 ) {
        $("#Sites").fill(json);
    }
    else {
        var txt1="";
        txt1+="<div id=\"SitesRow1\"><img src=\"/js/facebox/loading.gif\" /></div>\n";
        txt1+="<div id=\"SitesRow2\"><img src=\"/js/facebox/loading.gif\" /></div>\n";
        txt1+="<div id=\"SitesRow3\"><img src=\"/js/facebox/loading.gif\" /></div>\n";
        txt1+="<div id=\"SitesRow4\"></div>\n";
        $("#Sites").html(txt1);
        $("#Sites").fill(json);
    }
}

Client.prototype.onDocs=function(Id,result) {
    var txt="";
    txt+="<div onclick=\"client.Popup('Docs',this.parentNode.id,this.className,'" + Id + "');\" class=\"doc-resultreturn\">";
    txt+="<p class=\"title\">" + result.Title + "</p>";
    txt+="<p>" + result.Message + "</p>";
    txt+="</div>";

    var json = {};
    json['DocsRow'+result.Row] = txt;

    if( $("#DocsRow1").length > 0 ) {
        $("#Docs").fill(json);
    }
    else {
        var txt1="";
        txt1+="<div id=\"DocsRow1\"><img src=\"/js/facebox/loading.gif\" /></div>\n";
        txt1+="<div id=\"DocsRow2\"><img src=\"/js/facebox/loading.gif\" /></div>\n";
        txt1+="<div id=\"DocsRow3\"><img src=\"/js/facebox/loading.gif\" /></div>\n";
        txt1+="<div id=\"DocsRow4\"></div>\n";
        $("#Docs").html(txt1);
        $("#Docs").fill(json);
    }
}

Client.prototype.onVideos=function(Id,result) {
    var txt="";
    txt+="<div class=\"video-resultreturn\" onclick=\"client.Popup('Video',this.parentNode.id,this.className,'" + Id + "');\">";
    txt+="<p class=\"title\">" + result.Title + "</p>";
    txt+="<img src=\"" + result.ImgUrl + "\" class=\"screenshot\"/>";
    txt+="<p class\"site\">" + result.Site + "</p>";
    txt+="</div>";

    var json = {};
    json['VideosRow'+result.Row] = txt;

    if( $("#VideosRow1").length > 0 ) {
        $("#Videos").fill(json);
    }
    else {
        var txt1="";
        txt1+="<div id=\"VideosRow1\"><img src=\"/js/facebox/loading.gif\" /></div>\n";
        txt1+="<div id=\"VideosRow2\"><img src=\"/js/facebox/loading.gif\" /></div>\n";
        txt1+="<div id=\"VideosRow3\"><img src=\"/js/facebox/loading.gif\" /></div>\n";
        txt1+="<div id=\"VideosRow4\"></div>\n";
        $("#Videos").html(txt1);
        $("#Videos").fill(json);
    }
}

Client.prototype.onYahooAnswers=function(Id,result) {
    var txt="";
    txt+="<div class=\"yahoo-resultreturn\" onclick=\"location=client.Popup('YahooAnswers',this.parentNode.id,this.className,'" + Id + "')\">";
    txt+="<p class=\"question\">" + result.Question + "</p>";
    txt+="<p class=\"details\">" + result.Details + "</p>\n";
    txt+="<p>" + result.Body + "</p>\n";
    txt+="</div>";

    var json = {};
    json['YahooAnswersRow'+result.Row] = txt;

    if( $("#YahooAnswersRow1").length > 0 ) {
        $("#YahooAnswers").fill(json);
    }
    else {
        var txt1="";
        txt1+="<div id=\"YahooAnswersRow1\"><img src=\"/js/facebox/loading.gif\" /></div>\n";
        txt1+="<div id=\"YahooAnswersRow2\"><img src=\"/js/facebox/loading.gif\" /></div>\n";
        txt1+="<div id=\"YahooAnswersRow3\"><img src=\"/js/facebox/loading.gif\" /></div>\n";
        txt1+="<div id=\"YahooAnswersRow4\"></div>\n";
        $("#YahooAnswers").html(txt1);
        $("#YahooAnswers").fill(json);
    }
}

Client.prototype.AddResults=function(Id, Results) {
    eval(Results.Function)(Id, Results);
    this.refreshCols();
}

/* * * * * * * * * * * * *
 * Function: getUserDetails(obj, username)
 *
 * Desc: this function is a json call that determines if the requested username is already a friend of the one who called it.
 * Otherwise to display a friend request form.
 *
 * Statements:  obj --> this gets the chat's ID so that when we get a result we can return a json with a div displaying the information to it.
 *    username --> this is the requested username which we use to get the required information prior to sending the results
 * * * * * * * * * * * * */
function getUserDetails(obj, un, img) {
    if( $("#"+obj+" .chatbox_serverMisc .userDetailWrapperBox").length > 0 ) {
        $("#"+obj+" .chatbox_serverMisc .userDetailWrapperBox").toggle();
    }
    else {
        var txt = "";
        txt+="<div class=\"userDetailWrapperBox\">\n";
        txt+="<ul class=\"udMenu\">\n";
        txt+="<li><img src=\"/images/udAddFriend.png\" alt=\"\" /> Follow</li>\n";
        txt+="<li onclick=\"$('#" + obj + " .chatbox_serverMisc .userDetailWrapperBox').hide();requestFriend('"+un+"');\"><img src=\"/images/udAddFriend.png\" alt=\"\" /> Friend</li>\n";
        txt+="<li class=\"udMoreli\" onclick=\"toggleMoreOptions(this, '"+ obj +"');\">More Options <img src=\"/images/udFriendDropdown1.png\" alt=\"\" style=\"float:none !important\"/></li>\n";
        txt+="</ul>\n"
        txt+="<div class=\"udUserInfo\"><img src=\"/images/udOnlineUser.png\" alt=\"\" /> "+ un +" <p> I <3 *insert penguin*</p></div>\n";
        txt+="<div class=\"udUserImage\"><img src=\""+img+"\" alt=\"\" /></div>\n";
        txt+="<ul class=\"udSubMenu\" style=\"display:none;\">\n";
        txt+="<li onclick=\"$('#" + obj + " .chatbox_serverMisc .userDetailWrapperBox').hide();removeFriend('"+un+"');\"><img src=\"/images/udRemoveFriend.png\" alt=\"\"/> Remove Friend</li>\n";
        txt+="<li><img src=\"/images/udReportAbuse.png\" alt\"\" /> Report Abuse</li>\n";
        txt+="<li><img src=\"/images/udSeeAll.png\" alt=\"\"/> See All Threads</li>\n";
        txt+="<li><img src=\"/images/udProfile.png\" alt=\"\"/> View Profile</li>\n";
        txt+="</ul>\n";
        txt+="</div>\n";
        $("#"+obj+" .chatbox_serverMisc").append(txt);
    }
}

function toggleMoreOptions(obj, id){
  if (obj.className == "udMoreli") {
      obj.className = "udMoreliA";
  }
    else {
        obj.className = "udMoreli";
  }
    $('#' + id + ' .chatbox_serverMisc .userDetailWrapperBox .udSubMenu').toggle();
}

Client.prototype.AddChat=function(ChatMsg) {
    this.chatCounter++;
    var chatField="<div class=\"chatbox_message_wrapper\" onmouseover=\"highlightMe(this);\" onmouseout=\"unhighlightMe(this);\">\n";
    chatField+="<ul id=\"" + this.chatCounter + "\">";
    chatField+="<li class=\"chatbox_image\" onclick=\"getUserDetails(this.parentNode.id,'" + ChatMsg.username + "', '"+ChatMsg.ImgUrl+"');\"><img alt=\"\" src=\"" + ChatMsg.ImgUrl +"\" /></li>\n";
    chatField+="<li class=\"chatbox_message\"><span class=\"message_username\">" + ChatMsg.username + ": </span> "+ ChatMsg.body + "\n";
    chatField+="<li class=\"chatbox_serverMisc\"></li>\n";
    chatField+="</ul>\n";
    chatField+="</div>";
    $("#chatbox_feed").append(chatField);
    //$("#pane3").jScrollPane({showArrows:true, scrollbarWidth: 20,maintainPosition:false});
    if($("#chatbox_feed").html()!=null && $("#chatbox_feed").html().length>0) {
        //    $("#chatbox_feed")[0].scrollTo($("#chatbox_feed").html().length);
        document.getElementById('chatbox_feed').scrollTop=document.getElementById('chatbox_feed').scrollHeight;
    }
}


Client.prototype.AddModalChat=function(ChatMsg) {
    this.chatCounter++;
    var chatField="<div class=\"modal_chatbox_message_wrapper\" onmouseover=\"highlightMe(this);\" onmouseout=\"unhighlightMe(this);\">\n";
    chatField+="<ul id=\"" + this.chatCounter + "\">";
    chatField+="<li class=\"modal_userImg\" onclick=\"getUserDetails(this.parentNode.id,'" + ChatMsg.username + "', '"+ChatMsg.ImgUrl+"');\"><img alt=\"\" src=\"" + ChatMsg.ImgUrl +"\" /></li>\n";
    chatField+="<li class=\"modal_userText\"><span class=\"username\">" + ChatMsg.username + ": </span> "+ ChatMsg.body + "\n";
    chatField+="<li class=\"modal_serverMisc\"></li>\n";
    chatField+="</ul>\n";
    chatField+="</div>";
    $("#pane2").append(chatField);
    //$("#pane3").jScrollPane({showArrows:true, scrollbarWidth: 20,maintainPosition:false});
    //if($("#chatbox_feed").html()!=null && $("#chatbox_feed").html().length>0) {
        //    $("#chatbox_feed")[0].scrollTo($("#chatbox_feed").html().length);
    //    document.getElementById('chatbox_feed').scrollTop=document.getElementById('chatbox_feed').scrollHeight;
    //}
}

Client.prototype.SetType=function(n,resultType) {
    ActiveTypes[n]=resultType;
}

Client.prototype.ScrollTimeline=function(initialStartDate,initialEndDate,finalStartDate,finalEndDate,Data) {
    //Using the Data in JSON format do the scrolling
}

Client.prototype.AddTimelineTags=function(Tags) {
    for(tag in Tags)
    {
        //do something with tag.tag, tag.time,tag.importance,tag.item_apge
    }
}

Client.prototype.ShowSearchResults=function(Sr) {
    //
}

Client.prototype.onRoomChange=function(newRoom, past, future, total_users) {
    this.CurrentRoom=newRoom;
    $("#chatbox_feed").html("");
    $("#current_room_name").html(newRoom);
    this.ClearData();
    this.refreshCols();
    this.chatCounter=0;
    this.addPrevTopics(past);
    this.addFutureTopics(future); 
		this.setTotalUsers(total_users);
}

Client.prototype.SuggestTopic=function(topicSuggested,newRoom) {
    var msg="<div class=\"system_message\" style=\"background-color: rgb(162, 210, 255);\">\n";
    msg+="<p>Room "+ newRoom +" Suggested. <a href=\"javascript:;\" onclick=\"changeConversation('"+newRoom+"');updateFutureChats();\">Click to Enter</a></p>\n";
    msg+="</div>";

    $("#chatbox_feed").append(msg);

    if($("#chatbox_feed").html()!=null && $("#chatbox_feed").html().length>0) {
        document.getElementById('chatbox_feed').scrollTop=document.getElementById('chatbox_feed').scrollHeight;
    }

    $("#futureChatTimeline").html(newRoom);
    $("#futureChatTimeline").effect("pulsate", {times:2}, 1000);

    if ($("#"+newRoom).length == 0) {
        var topicField="";
        topicField+="<div id="+newRoom+" class=\"future_topics_wrapper\" onclick=\"changeConversation('"+newRoom+"');getFutureChats();\">";
        topicField+="<ul>";
        topicField+="<li class=\"future_topics_img\"><img src=\"\" alt=\"\" /></li>";
        topicField+="<li class=\"future_topics_cat\">" + newRoom + "</li>";
        // The onclick even doesn't exists yet so can be removed, it's just an idea.
        topicField+="<li class=\"future_topics_remove\"><img onclick=\"removeTopic(" + newRoom + ")\" src=\"/images/past-topics/close.png\" alt=\"remove\" title=\"remove\" /></li>";
        topicField+="</ul>";
        topicField+="</div>";
        topicField+="<div class=\"clear\"></div>";

        $("#future_topics_feed").append(topicField);
    }

    //$("#suggestor_message").val(topicSuggested);

    //$("#suggest_yes").click(function() {
    //    $("#room_suggestor").attr("visibility","hidden");
    //    changeRoom(this.CurrentRoom,newRoom);
    //});
    //$("#suggest_no").click(function() {
    //    $("#room_suggestor").attr("visibility","hidden");
    //});
    //$("#room_suggestor").attr("visibility","visible");
}

Client.prototype.userLeftChat=function(user) {
    var msg="<div class=\"system_message\" style=\"background-color: rgb(255, 224, 252);\">\n";
    msg+="<p>" + user + " has left the chat</p>\n";
    msg+="</div>";
    $("#chatbox_feed").append(msg);
    //$("#pane3").jScrollPane({showArrows:true, scrollbarWidth: 20,maintainPosition:false});
    if($("#chatbox_feed").html()!=null && $("#chatbox_feed").html().length>0) {
        //   $("#chatbox_feed")[0].scrollTo($("#chatbox_feed").html().length);
        document.getElementById('chatbox_feed').scrollTop=document.getElementById('chatbox_feed').scrollHeight;
    }
    this.TotalUsers=this.TotalUsers-1;
    $("#num_users").html(this.TotalUsers);
}

Client.prototype.userEnteredChat=function(user) {
    var msg="<div class=\"system_message\" style=\"background-color: rgb(209, 255, 202);\">\n";
    msg+="<p>" + user + " has entered the chat</p>\n";
    msg+="</div>";
    $("#chatbox_feed").append(msg);
    //$("#pane3").jScrollPane({showArrows:true, scrollbarWidth: 20,maintainPosition:false});
    if($("#chatbox_feed").html()!=null && $("#chatbox_feed").html().length>0) {
        //    $("#chatbox_feed")[0].scrollTo($("#chatbox_feed").html().length);
        document.getElementById('chatbox_feed').scrollTop=document.getElementById('chatbox_feed').scrollHeight;
    }
    this.TotalUsers=this.TotalUsers+1;
    $("#num_users").html(this.TotalUsers);

}

Client.prototype.addFutureTopics=function(futureTopic) {
    var topicField="";

    $("#futureChatTimeline").html("");

    for(n in futureTopic) {
        if (n == 0) {
            $("#futureChatTimeline").html(futureTopic[0].topicName);
        }
        topicField+="<div class=\"future_topics_wrapper\" onclick=\"changeConversation('"+futureTopic[n].topicName+"');getFutureChats();\">";
        topicField+="<ul>";
        topicField+="<li class=\"future_topics_img\"><img src=\"" + futureTopic[n].ImgUrl +"\" alt=\"\" /></li>";
        topicField+="<li class=\"future_topics_cat\">" + futureTopic[n].topicCat +" - " + futureTopic[n].topicName + "</li>";
        // The onclick even doesn't exists yet so can be removed, it's just an idea.
        topicField+="<li class=\"future_topics_remove\"><img onclick=\"removeTopic(" + futureTopic[n].id + ")\" src=\"/images/past-topics/close.png\" alt=\"remove\" title=\"remove\" /></li>";
        topicField+="</ul>";
        topicField+="</div>";
        topicField+="<div class=\"clear\"></div>";
    }
    $("#future_topics_feed").html(topicField);
}

function userRegistrationRequest() {
    var txt="";
txt+="<div id=\"userRegRequest\" class=\"userRegRequest\">\n";
txt+="<form action=\"\">\n";
txt+="<p class=\"userRegWhiteP\">At minimum, create a username so people take you seriously</p>\n";
txt+="<input type=\"text\" id=\"regTempUser\" name=\"regTempUser\" class=\"regTempUser\" />\n";
txt+="<a href=\"javascript:;\" id=\"checkAvailability\" class=\"checkUsername\">check availability</a>\n";
txt+="<p class=\"userRegBlackP\"><a href=\"javascript:;\" id=\"toggleFullReg\">It'd be even better if you created a profile <img src=\"/images/bArrowDown.png\" alt=\"\"/></a></p>\n";
txt+="</div>\n";
txt+="<div id=\"userRegForm\" style=\"display:none;\">\n";
txt+="<form action=\"alert(\"meep\");\">\n";
txt+="<div class=\"regFormField\">\n";
txt+="<label for=\"regFullName\">Full Name</label>\n";
txt+="<input type=\"text\" name=\"regFullName\" id=\"regFullName\" class=\"regTextInput\" />\n";
txt+="</div>\n";
txt+="<div class=\"regFormField\">\n";
txt+="<label for=\"regPassword\">Password</label>\n";
txt+="<input type=\"password\" name=\"regPassword\" id=\"regPassword\" class=\"regTextInput\" />\n";
txt+="<div class=\"maxChar\">6 characters</div>\n";
txt+="</div>\n";
txt+="<div class=\"regFormField\">\n";
txt+="<label for=\"regEmail\">Email</label>\n";
txt+="<input type=\"text\" name=\"regEmail\" id=\"regEmail\" class=\"regTextInput\" />\n";
txt+="</div>\n";
txt+="<div class=\"regFormField\">\n";
txt+="<input type=\"submit\" name=\"regNewUserSubmit\" id=\"regNewUserSubmit\" class=\"regSubmitInput\" value=\"create account\" />\n";
txt+="</div>\n";
txt+="</form>\n";
txt+="</div>\n";
$("#userRegRequestWrapper").html(txt);
}

Client.prototype.addPrevTopics=function(prevTopic) {
    var topicField="";

    for(n in prevTopic) {
        if (n == 0) {
            $("#pastChatTimeline").html(prevTopic[0].topicName);
        }
        topicField+="<div class=\"past_topics_wrapper\" onclick=\"changeConversation('"+prevTopic[n].topicName+"');getPastChats();\">";
        topicField+="<ul>";
        topicField+="<li class=\"past_topics_img\"><img src=\"" + prevTopic[n].ImgUrl +"\" alt=\"\" /></li>";
        topicField+="<li class=\"past_topics_cat\">" + prevTopic[n].topicCat +" - " + prevTopic[n].topicName + "</li>";
        // The onclick even doesn't exists yet so can be removed, it's just an idea.
        topicField+="<li class=\"past_topics_remove\"><img onclick=\"removeTopic(" + prevTopic[n].id + ")\" src=\"/images/past-topics/close.png\" alt=\"remove\" title=\"remove\" /></li>";
        topicField+="</ul>";
        topicField+="</div>";
        topicField+="<div class=\"clear\"></div>";
    }
    $("#past_topics_feed").html(topicField);
}

Client.prototype.setTopics=function(topics) {
    this.Topics=topics;
}

Client.prototype.getTopics=function() {
    return(this.Topics);
}

Client.prototype.onChatSearched=function(searchTerm,jsonResults) {
}

var client=new Client();
