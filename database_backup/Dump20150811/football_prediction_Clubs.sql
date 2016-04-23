CREATE DATABASE  IF NOT EXISTS `football_prediction` /*!40100 DEFAULT CHARACTER SET utf8 */;
USE `football_prediction`;
-- MySQL dump 10.13  Distrib 5.5.44, for debian-linux-gnu (x86_64)
--
-- Host: 127.0.0.1    Database: football_prediction
-- ------------------------------------------------------
-- Server version	5.5.44-0ubuntu0.14.04.1

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `Clubs`
--

DROP TABLE IF EXISTS `Clubs`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `Clubs` (
  `idClubs` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(45) DEFAULT NULL,
  `country` varchar(45) DEFAULT NULL,
  `city` varchar(45) DEFAULT NULL,
  `name_in_football_data` varchar(45) DEFAULT NULL,
  `id_in_xml_soccer` int(11) DEFAULT NULL,
  `name_in_xml_soccer` varchar(45) DEFAULT NULL,
  PRIMARY KEY (`idClubs`)
) ENGINE=InnoDB AUTO_INCREMENT=113 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `Clubs`
--

LOCK TABLES `Clubs` WRITE;
/*!40000 ALTER TABLE `Clubs` DISABLE KEYS */;
INSERT INTO `Clubs` VALUES (1,'Malaga','Spain','Malaga','Malaga',NULL,NULL),(2,'Athletic Bilbao','Spain','Bilbao','Ath Bilbao',NULL,NULL),(3,'Valencia','Spain','Walencja','Valencia',NULL,NULL),(4,'Racing Santander','Spain','Santander','Santander',NULL,NULL),(5,'Espanol','Spain','Barcelona','Espanol',NULL,NULL),(6,'Deportivo Alaves','Spain','Vitoria','Alaves',NULL,NULL),(7,'Real Valladolid','Spain','Valladolid','Valladolid',NULL,NULL),(8,'Real Oviedo','Spain','Oviedo','Oviedo',NULL,NULL),(9,'Celta','Spain','Vigo','Celta',NULL,NULL),(10,'Rayo Vallecano','Spain','Madryt','Vallecano',NULL,NULL),(11,'Barcelona','Spain','Barcelona','Barcelona',NULL,NULL),(12,'Real Madrid','Spain','Madryt','Real Madrid',NULL,NULL),(13,'Deportvo La Coruna','Spain','La Coruna','La Coruna',NULL,NULL),(14,'Real Mallorca','Spain','Mallorca','Mallorca',NULL,NULL),(15,'Numancia','Spain','Soria','Numancia',NULL,NULL),(16,'Villarreal','Spain','Villarreal','Villarreal',NULL,NULL),(17,'Real Sociedad','Spain','Sociedad','Sociedad',NULL,NULL),(18,'Osasuna','Spain','Pampeluna','Osasuna',NULL,NULL),(19,'Las Palmas','Spain','Las Palmas','Las Palmas',NULL,NULL),(20,'Real Zaragoza','Spain','Zaragoza','Zaragoza',NULL,NULL),(21,'Betis','Spain','Sevilla','Betis',NULL,NULL),(22,'Tenerife','Spain','Tenerife','Tenerife',NULL,NULL),(23,'Sevilla','Spain','Sevilla','Sevilla',NULL,NULL),(24,'Athletico Madrid','Spain','Madryt','Ath Madrid',NULL,NULL),(25,'Recreativo Huelva','Spain','Huelva','Recreativo',NULL,NULL),(26,'Murcia','Spain','Murcia','Murcia',NULL,NULL),(27,'Albacete Balompié','Spain','Albacete','Albacete',NULL,NULL),(28,'Levante','Spain','Walencja','Levante',NULL,NULL),(29,'Getafe','Spain','Getafe','Getafe',NULL,NULL),(30,'Cadiz','Spain','Cadiz','Cadiz',NULL,NULL),(31,'Gimnàstic Tarragona','Spain','Gimnastic','Gimnastic',NULL,NULL),(32,'Ud Almeria','Spain','Almeria','Almeria',NULL,NULL),(33,'Sporting Gijon','Spain','Gijon','Sp Gijon',NULL,NULL),(34,'Xerez CD','Spain','Jerez de la Frontera','Xerez',NULL,NULL),(35,'Hercules Alicante','Spain','Alicante','Hercules',NULL,NULL),(36,'Granada CF','Spain','Granada','Granada',NULL,NULL),(37,'Elche CF','Spain','Elche','Elche',NULL,NULL),(38,'CD Logrones','Spain','Logrono','Logrones',NULL,NULL),(39,'Lerida','Spain','Lerida','Lerida',NULL,NULL),(40,'SD Compostela','Spain','Santiago de Compostela','Compostela',NULL,NULL),(41,'UD Salamanca','Spain','Salamanca','Salamanca',NULL,NULL),(42,'Merida AD','Spain','Merida','Merida',NULL,NULL),(43,'Extremadura UD','Spain','Almendralejo','Extremadura',NULL,NULL),(44,'Villareal','Spain','Villareal','Villareal',NULL,NULL),(64,'Manchester City','England','Manchester','Man City',NULL,NULL),(65,'West Ham United','England','Londyn','West Ham',NULL,NULL),(66,'Middlesbrough','England','Middlesbrough','Middlesbrough',NULL,NULL),(67,'Southampton','England','Southampton','Southampton',NULL,NULL),(68,'Everton','England','Liverpool','Everton',NULL,NULL),(69,'Aston Villa','England','Birmingham','Aston Villa',NULL,NULL),(70,'Bradford City','England','Bradford','Bradford',NULL,NULL),(71,'Arsenal','England','Londyn','Arsenal',NULL,NULL),(72,'Ipswich Town','England','Ipswich','Ipswich',NULL,NULL),(73,'Newcastle','England','Newcastle','Newcastle',NULL,NULL),(74,'Liverpool','England','Liverpool','Liverpool',NULL,NULL),(75,'Chelsea','England','Londyn','Chelsea',NULL,NULL),(76,'Manchester United','England','Manchester','Man United',NULL,NULL),(77,'Tottenham','England','Londyn','Tottenham',NULL,NULL),(78,'Charlton Athletic','England','Charlton','Charlton',NULL,NULL),(79,'Sunderland','England','Sunderland','Sunderland',NULL,NULL),(80,'Derby Country','England','Derby','Derby',NULL,NULL),(81,'Coventry City','England','Coventry','Coventry',NULL,NULL),(82,'Leicester','England','Leicester','Leicester',NULL,NULL),(83,'Leeds United','England','Leeds','Leeds',NULL,NULL),(84,'Blackburn Rovers','England','Blackburn','Blackburn',NULL,NULL),(85,'Bolton Wanderers','England','Bolton','Bolton',NULL,NULL),(86,'Fulham','England','Fulham','Fulham',NULL,NULL),(87,'West Bromwich Albion','England','West Bromwich','West Brom',NULL,NULL),(88,'Middlesbrough','England','Middlesbrough','Middlesboro',NULL,NULL),(89,'Birmingham City','England','Birmingham','Birmingham',NULL,NULL),(90,'Wolverhampton Wanderers','England','Wolverhampton','Wolves',NULL,NULL),(91,'Portsmouth','England','Portsmouth','Portsmouth',NULL,NULL),(92,'Crystal Palace','England','Londyn','Crystal Palace',NULL,NULL),(93,'Norwich City','England','Norwich','Norwich',NULL,NULL),(94,'Wigan Athletic','England','Wigan','Wigan',NULL,NULL),(95,'Watford','England','Watford','Watford',NULL,NULL),(96,'Sheffield United','England','Sheffield','Sheffield United',NULL,NULL),(97,'Reading','England','Reading','Reading',NULL,NULL),(98,'Stoke City','England','Stoke','Stoke',NULL,NULL),(99,'Hull City','England','Hull','Hull',NULL,NULL),(100,'Burnley','England','Burnley','Burnley',NULL,NULL),(101,'Blackpool','England','Blackpool','Blackpool',NULL,NULL),(102,'Swansea','England','Swansea','Swansea',NULL,NULL),(103,'Queens Park Rangers','England','Londyn','QPR',NULL,NULL),(104,'Cardiff City','England','Cardiff','Cardiff',NULL,NULL),(105,'Sheffield Wednesday','England','Sheffield','Sheffield Weds',NULL,NULL),(106,'Swindon','England','Swindon','Swindon',NULL,NULL),(107,'Wimbledon','England','Londyn','Wimbledon',NULL,NULL),(108,'Oldham Athletic','England','Oldham','Oldham',NULL,NULL),(109,'Nottingham Forest','England','Nottingham','Nott\'m Forest',NULL,NULL),(110,'Barnsley','England','Barnsley','Barnsley',NULL,NULL),(111,'CD Eibar','Spain','Eibar','Eibar',NULL,NULL),(112,'Cordoba CF','Spain','Kordowa','Cordoba',NULL,NULL);
/*!40000 ALTER TABLE `Clubs` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2015-08-11 18:18:33
