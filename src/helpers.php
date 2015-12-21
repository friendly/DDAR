<?php
/**
 * Get the BASEPATH env var
 * @return string
 */
function basepath() {
    $basepath = getenv('BASEPATH');
    if (!$basepath) {
        return '';
    }
    return rtrim($basepath, '/');
}

/**
 * Build a url for the static path
 * @param string $path
 * @return string
 */
function urlfor($path) {
    return basepath() . $path;
}

/**
 * Build the chapter navigation sub pages
 * @param string $path
 * @return string
 */
function renderChapterNavigation($path) {
    $objects = new RecursiveIteratorIterator(
        new RecursiveDirectoryIterator($path),
        RecursiveIteratorIterator::SELF_FIRST
    );
    $items = array();
    foreach ($objects as $o) {
        $filename = $o->getFileName();
        $re = '/ch([0-9]{1,2})\.html/';
        preg_match($re, $filename, $matches);
        if (count($matches) == 2) {
            $filename = basename($filename, '.html');
            $filepath = '/' . $path . '/' . $filename;
            $items[$matches[1]] = $filepath;
        }
    }
    ksort($items);
    $html = '';
    foreach ($items as $num=>$href) {
        $html .=
            '<li>' .
            '<a class="page chapter"' .
            ' href="' . $href  . '">' .
            'Chapter ' . $num .
            '</a>' .
            '</li>' . PHP_EOL;

    }
    return $html;
}
