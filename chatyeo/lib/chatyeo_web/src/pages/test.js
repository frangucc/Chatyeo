function runTest() {
	client.onImage([{Url:"/",ImgUrl:"img/userimg-001.jpg",Title:"Machine learning ad network scientist for SAAS."}]);
	client.onTextFinding([{Title:"Paul Dix Hosts 2009 Semantic Meetup: Machine Learning",Description:"Some parts of machine learning are closely related to data mining and statistics. Machine learning research is focused on computational properties of the statistical methods, such as their computational complexity..."},
		     {Title:"Henry Wikler gives talk at Semantic Meetup",
		      Description:"Hence, machine learning is closely related to fields such as statistics, pattern recognition, artificial... MLMTA Machine Learning: MOdels, Technologies & Applications. Much label classification. Neural Information Processing systems(NIPS)."},{Title:"ML Conference kickoff next week. Please attend...",Description:"of machine learning are closely related to data mining and statistics. Machine learning research is focused on computational properties of the statistical methods, such as their computational complexity..."}]
	);
	client.AddChat({ImgUrl:"img/logo.png",username:"gersh",body:"Hello, this is a test message"});
}
setTimeout(runTest,2000);

