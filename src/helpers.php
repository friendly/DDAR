<?php
/**
 * Build the chapter navigation sub pages
 * @param string $path
 * @return string
 */
function getChapterPageNavHtml($path) {
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
            $filepath = $path . '/' . $filename;
            $items[$matches[1]] = $filepath;
        }
    }
    ksort($items);
    $html = '';
    foreach ($items as $k=>$v) {
        $html .=
            '<li>' .
            '<a class="page chapter"' .
            ' href="/' . $v  . '">' .
            'Chapter ' . $k .
            '</a>' .
            '</li>' . PHP_EOL;

    }
    return $html;
}
