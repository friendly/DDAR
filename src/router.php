<?php
require 'vendor/autoload.php';
require_once 'src/helpers.php';

/**
 * A custom router built on top of AltoRouter
 *
 * @author Arietis Software <code@arietis-software.com>
 * @version 1.0
 */
class DDARRouter
{
    private $router;
    private $basepath;

    /**
     * Constructor
     * @param string $basepath
     */
    public function __construct($basepath) {
        $this->basepath = $basepath;
        $this->router = new AltoRouter();
    }

    /**
     * Get the BASEPATH
     * @return string
     */
    public function getBasepath() {
        return $this->basepath;
    }

    /**
     * Load the router and its routes
     */
    public function load() {
        if (!empty($this->basepath)) {
            $this->router->setBasePath($this->basepath);
        }
        //set catch-all route
        $this->router->map('GET', '/[*:path]', function(){});
    }

    /**
     * Perform a routing match and load the content
     * @return string
     */
    public function match() {
        $match = $this->router->match();
        return $this->getPageContent($match);
    }

    /**
     * Get the page content as a string
     * @param object $match
     * @return string
     */
    private function getPageContent($match) {
        $params = $match['params'];
        $path = rtrim($params['path'], '/');

        if (empty($path)) { //home
            $page = 'pages/home.html';
        } else {
            $page = $path . '.html';
            if (!is_file($page)) { //error
                $page = 'pages/error.html';
            }
        }
        return file_get_contents($page);
    }
}
