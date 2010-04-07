//package com.defyned.chatyeo.lucene;
package org.apache.lucene.index;

import org.apache.lucene.index.*;
import org.apache.lucene.search.*;
import org.apache.lucene.store.*;
import org.apache.lucene.search.*;
import java.io.*;
import java.util.*;
import org.apache.lucene.analysis.*;
import org.apache.lucene.document.*;

public class SearcherManager {
	public static int WAIT_TIME = 10000;
	private IndexSearcher[] searchers;
	private Directory[] directories;
	private MultiSearcher currentSearcher;
	private boolean reopening;
	private UpdaterThread updater;
	private int ref[];
	private Boolean atomic = new Boolean(false);


	private class UpdaterThread extends Thread{
		public void run()
		{
			while(true){
				try{
					this.sleep(WAIT_TIME);
					maybeReopen();
				}catch(Exception e){
					e.printStackTrace();
				}
			}

		}
	}

	public SearcherManager(Directory[] dirs) throws IOException,InterruptedException
	{
		this.directories = dirs;
		searchers = new IndexSearcher[directories.length];
		for(int i = 0; i < directories.length; i++)
			searchers[i] = new IndexSearcher(IndexReader.open(directories[i]));
		currentSearcher = new MultiSearcher(searchers);
		updater = new UpdaterThread();
		updater.start();
		ref = new int[dirs.length];
	}

	public void warm(MultiSearcher searcher) {}
	private synchronized void lock()
		throws InterruptedException {
		while (reopening) {
			wait();
		}
		reopening = true;
	}

	private synchronized void unlock() {
		reopening = false;
		notifyAll();
	}

	public void maybeReopen() throws InterruptedException, IOException { 
		lock();
		try {
			final MultiSearcher _searcher = get();
			try {
				boolean changed = false;

				for(int i = 0; i < directories.length; i++)
				{
					if(IndexReader.getCurrentVersion(directories[i]) != searchers[i].getIndexReader().getVersion()){
						IndexReader newReader = searchers[i].getIndexReader().reopen();
						IndexSearcher newSearcher = new IndexSearcher(newReader);
						IndexSearcher tempSearcher = searchers[i];

						searchers[i] = newSearcher;
						tempSearcher.getIndexReader().decRef();

						changed = true;
					}
					
				}
				if(changed){
					currentSearcher = new MultiSearcher(searchers);
				}
			} finally {
				release(_searcher);
			}
		} finally {
			unlock();
		}
	}

	public synchronized MultiSearcher get() throws InterruptedException
	{
		MultiSearcher m;
		synchronized(atomic){
			m = currentSearcher;
			Searchable[] s = m.getSearchables();
			for(int i =0; i < s.length; i++){
				IndexSearcher j = (IndexSearcher)s[i];
				j.getIndexReader().incRef();
			}
		}
		return m;
	}
	public synchronized void release(MultiSearcher searcher) throws InterruptedException, IOException {
		synchronized(atomic){
			Searchable[] s = searcher.getSearchables();
			for(int i = 0; i < s.length; i++){
				IndexSearcher j = (IndexSearcher)s[i];
				j.getIndexReader().decRef();
			}
		}
	}
	
	public static void main(String[] args) throws Exception
	{
		IndexWriter[] writers;
		Directory[] directories;
		Analyzer analyzer;

		writers = new IndexWriter[2];
		directories = new Directory[2];
		analyzer = new SimpleAnalyzer();


		directories[0] = new RAMDirectory();
		directories[1] = FSDirectory.getDirectory("index/");

		writers[0] = new IndexWriter(directories[0],analyzer);
		writers[1] = new IndexWriter(directories[1],analyzer);


		SearcherManager s = new SearcherManager(directories);
		Document d = new Document();
		d.add(new Field("field", "There is some text that goes here. Please search me", Field.Store.YES, Field.Index.ANALYZED));
		writers[0].addDocument(d);
		writers[0].close();

		int i = 0;
		while(i++ >= 0)
		{
			MultiSearcher m = s.get();
			Query q = new TermQuery(new Term("field","search"));
			TopDocs docs = m.search(q,10);
			System.out.println("Total Hits: " + docs.totalHits);
			Thread.currentThread().sleep(1000);
			s.release(m);
		}

		
	}
}

