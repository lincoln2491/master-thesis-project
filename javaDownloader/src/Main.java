import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.xml.sax.SAXException;

public class Main {

	static String urlString = "http://www.xmlsoccer.com/FootballData.asmx/GetHistoricMatchesByLeagueAndSeason?ApiKey=QULJJPHESUGHJJENBKKKDFOKSUZYXMMADREQVDHJEUZYDBSBMR&league=";

	static String dates[] = { "0001", "0102", "0203", "0304", "0405", "0506",
			"0607", "0708", "0809", "0910", "1011", "1112", "1213", "1314",
			"1415",

	};

	public static void main(String[] args) throws IOException,
			ParserConfigurationException, SAXException, TransformerException, InterruptedException {
		// TODO Auto-generated method stub

		for (int i = 3; i < 7; i++) {
			Thread.sleep(1000 * 3700);
			DateFormat dateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
			Date date = new Date();
			System.out.println(dateFormat.format(date) + " SP " + dates[i]); 
			String tmp = createString("8", dates[i]);
			System.out.println(tmp);

			URL url = new URL(tmp);
			URLConnection conn = url.openConnection();

			DocumentBuilderFactory factory = DocumentBuilderFactory
					.newInstance();
			DocumentBuilder builder = factory.newDocumentBuilder();
			Document doc = builder.parse(conn.getInputStream());

			TransformerFactory tfactory = TransformerFactory.newInstance();
			Transformer xform = tfactory.newTransformer();

			File myOutput = new File("/home/ksurdyk/tmp/sp" + dates[i] + ".xml");
			xform.transform(new DOMSource(doc), new StreamResult(myOutput));
			
		}
		
		for (int i = 0; i < 7; i++) {
			Thread.sleep(1000 * 3700);
			DateFormat dateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
			Date date = new Date();
			System.out.println(dateFormat.format(date) + " EN " + dates[i]); 
			String tmp = createString("1", dates[i]);
			System.out.println(tmp);

			URL url = new URL(tmp);
			URLConnection conn = url.openConnection();

			DocumentBuilderFactory factory = DocumentBuilderFactory
					.newInstance();
			DocumentBuilder builder = factory.newDocumentBuilder();
			Document doc = builder.parse(conn.getInputStream());

			TransformerFactory tfactory = TransformerFactory.newInstance();
			Transformer xform = tfactory.newTransformer();

			File myOutput = new File("/home/ksurdyk/tmp/en" + dates[i] + ".xml");
			xform.transform(new DOMSource(doc), new StreamResult(myOutput));
		}
		
		
		
		
		
		
		for (int i = 7; i < dates.length; i++) {
			Thread.sleep(1000 * 3700);
			DateFormat dateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
			Date date = new Date();
			System.out.println(dateFormat.format(date) + " SP " + dates[i]); 
			String tmp = createString("8", dates[i]);
			System.out.println(tmp);

			URL url = new URL(tmp);
			URLConnection conn = url.openConnection();

			DocumentBuilderFactory factory = DocumentBuilderFactory
					.newInstance();
			DocumentBuilder builder = factory.newDocumentBuilder();
			Document doc = builder.parse(conn.getInputStream());

			TransformerFactory tfactory = TransformerFactory.newInstance();
			Transformer xform = tfactory.newTransformer();

			File myOutput = new File("/home/ksurdyk/tmp/sp" + dates[i] + ".xml");
			xform.transform(new DOMSource(doc), new StreamResult(myOutput));
			
		}
		
		for (int i = 7; i < dates.length; i++) {
			Thread.sleep(1000 * 3700);
			DateFormat dateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
			Date date = new Date();
			System.out.println(dateFormat.format(date) + " EN " + dates[i]); 
			String tmp = createString("1", dates[i]);
			System.out.println(tmp);

			URL url = new URL(tmp);
			URLConnection conn = url.openConnection();

			DocumentBuilderFactory factory = DocumentBuilderFactory
					.newInstance();
			DocumentBuilder builder = factory.newDocumentBuilder();
			Document doc = builder.parse(conn.getInputStream());

			TransformerFactory tfactory = TransformerFactory.newInstance();
			Transformer xform = tfactory.newTransformer();

			File myOutput = new File("/home/ksurdyk/tmp/en" + dates[i] + ".xml");
			xform.transform(new DOMSource(doc), new StreamResult(myOutput));
		}

	}

	static String createString(String league, String season) {
		return urlString + league + "&seasonDateString=" + season;
	}

}
