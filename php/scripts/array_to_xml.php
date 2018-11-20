<?php
/**
 * Created by PhpStorm.
 * User: jbrahy
 * Date: 10/3/18
 * Time: 15:37
 */

/*
<?xml version="1.0" encoding="UTF-8"?>
	<Popular_Campaigns>
	<row no="1">
		<FL val="Campaign Name">{$data['internal_name']}</FL>
		<FL val="Campaign ID">{$data['sid']}</FL>
		<FL val="Advertiser id">{$data['advertiser_id']}</FL>
		<FL val="Advertiser">{$data['company']}</FL>
		<FL val="Campaign Owner">{$data['first_name']} {$data['last_name']}</FL>
		<FL val="Campaign Description"></FL>
		<FL val="Extra Notice"></FL>
		<FL val="Channels"></FL>
		<FL val="Currency Campaign">{$currency}</FL>
		<FL val="Campaign Type">{$data['approval_type']}</FL>
		<FL val="Pay in">{$data['payin']}</FL>
		<FL val="Payout">{$data['payout']}</FL>
		<FL val="Status">{$data['status']}</FL>
		<FL val="Main Landing Page"></FL>
		<FL val="Categories"></FL>
	</row>
</Popular_Campaigns>
 */

$array = array(
	'row' => array(
		'@no' => 1,
		'FL' => array(
			0 => array(
				'@val' => 'Campaign Name',
				'%' => 'Campaign Value',
			),
			1 => array(
				'@val' => 'Campaign ID',
				'%' => 'Campaign Value',
			),
			2 => array(
				'@val' => 'Advertiser id',
				'%' => 'Campaign Value',
			),
			3 => array(
				'@val' => 'Advertiser',
				'%' => 'Campaign Value',
			),
			4 => array(
				'@val' => 'Campaign Owner',
				'%' => 'Campaign Value',
			),
			5 => array(
				'@val' => 'Campaign Description',
				'%' => 'Campaign Value',
			),
			6 => array(
				'@val' => 'Extra Notice',
				'%' => 'Campaign Value',
			),
			7 => array(
				'@val' => 'Channels',
				'%' => 'Campaign Value',
			),
			8 => array(
				'@val' => 'Currency Campaign',
				'%' => 'Campaign Value',
			),
			9 => array(
				'@val' => 'Campaign Type',
				'%' => 'Campaign Value',
			),
			10 => array(
				'@val' => 'Pay In',
				'%' => 'Campaign Value',
			),
			11 => array(
				'@val' => 'Payout',
				'%' => 'Campaign Value',
			),
			12 => array(
				'@val' => 'Status',
				'%' => 'Campaign Value',
			),
			13 => array(
				'@val' => 'Main Landing Page',
				'%' => 'Campaign Value',
			),
			14 => array(
				'@val' => 'Categories',
				'%' => 'Campaign Value',
			),
		),
	),
);
$array_to_xml = new ArrayToXML();

$xml = @$array_to_xml->buildXML($array, 'Popular_Campaigns');

printf("%s\n", $xml);

class ArrayToXML {

	private $version;
	private $encoding;

	/**
	 * Construct ArrayToXML object with selected version and encoding
	 *
	 * for available values check XmlWriter docs http://www.php.net/manual/en/function.xmlwriter-start-document.php
	 *
	 * @param string $xmlVersion XML Version, default 1.0
	 * @param string $xmlEncoding XML Encoding, default UTF-8
	 */
	public function __construct($xmlVersion = '1.0', $xmlEncoding = 'UTF-8') {

		$this->version = $xmlVersion;
		$this->encoding = $xmlEncoding;
	}

	/**
	 * Build an XML Data Set
	 *
	 * @param array  $data Associative Array containing values to be parsed into an XML Data Set(s)
	 * @param string $startElement Root Opening Tag, default data
	 *
	 * @return string XML String containing values
	 * @return mixed Boolean false on failure, string XML result on success
	 */
	public function buildXML($data, $startElement = 'data') {

		if (!is_array($data)) {
			$err = 'Invalid variable type supplied, expected array not found on line ' . __LINE__ . ' in Class: ' . __CLASS__ . ' Method: ' . __METHOD__;
			trigger_error($err);

			return FALSE; //return false error occurred
		}
		$xml = new XmlWriter();
		$xml->openMemory();
		$xml->startDocument($this->version, $this->encoding);
		$xml->startElement($startElement);

		$data = $this->writeAttr($xml, $data);
		$this->writeEl($xml, $data);

		$xml->endElement(); //write end element
		//returns the XML results
		return $xml->outputMemory(TRUE);
	}

	/**
	 * Write keys in $data prefixed with @ as XML attributes, if $data is an array.
	 * When an @ prefixed key is found, a '%' key is expected to indicate the element itself,
	 * and '#' prefixed key indicates CDATA content
	 *
	 * @param XMLWriter $xml object
	 * @param array     $data with attributes filtered out
	 *
	 * @return array $data | $nonAttributes
	 */
	protected function writeAttr(XMLWriter $xml, $data) {

		if (is_array($data)) {
			$nonAttributes = array();
			foreach ($data as $key => $value) {
				//handle an attribute with elements
				if ($key[0] == '@') {
					$xml->writeAttribute(substr($key, 1), $value);
				} else if ($key[0] == '%') {
					if (is_array($value)) {
						$nonAttributes = $value;
					} else {
						$xml->text($value);
					}
				} elseif ($key[0] == '#') {
					if (is_array($value)) {
						$nonAttributes = $value;
					} else {
						$xml->startElement(substr($key, 1));
						$xml->writeCData($value);
						$xml->endElement();
					}
				} else if ($key[0] == "!") {
					if (is_array($value)) {
						$nonAttributes = $value;
					} else {
						$xml->writeCData($value);
					}
				} //ignore normal elements
				else {
					$nonAttributes[$key] = $value;
				}
			}

			return $nonAttributes;
		} else {
			return $data;
		}
	}

	/**
	 * Write XML as per Associative Array
	 *
	 * @param XMLWriter $xml object
	 * @param array     $data Associative Data Array
	 */
	protected function writeEl(XMLWriter $xml, $data) {

		foreach ($data as $key => $value) {
			if (is_array($value) && !$this->isAssoc($value)) { //numeric array
				foreach ($value as $itemValue) {
					if (is_array($itemValue)) {
						$xml->startElement($key);
						$itemValue = $this->writeAttr($xml, $itemValue);
						$this->writeEl($xml, $itemValue);
						$xml->endElement();
					} else {
						$itemValue = $this->writeAttr($xml, $itemValue);
						$xml->writeElement($key, "$itemValue");
					}
				}
			} else if (is_array($value)) { //associative array
				$xml->startElement($key);
				$value = $this->writeAttr($xml, $value);
				$this->writeEl($xml, $value);
				$xml->endElement();
			} else { //scalar
				$value = $this->writeAttr($xml, $value);
				$xml->writeElement($key, "$value");
			}
		}
	}

	/**
	 * Check if array is associative with string based keys
	 * FROM:
	 * http://stackoverflow.com/questions/173400/php-arrays-a-good-way-to-check-if-an-array-is-associative-or-sequential/4254008#4254008
	 *
	 * @param array $array Array to check
	 *
	 * @return bool
	 */
	protected function isAssoc($array) {

		return (bool) count(array_filter(array_keys($array), 'is_string'));
	}
}