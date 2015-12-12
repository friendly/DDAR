<?php
function getNavContentPagesHtml($inDir, $webpath) {
    $html = '';
    $dir = realpath($inDir);
    if (!$inDir) {
        throw new Exception('Found no content html pages in ' . $inDir);
    }
    $re = '/ch([0-9]{1,2})\.html/';
    chdir($dir);
    foreach (glob('*.html') as $filename) {
        $basename = basename($filename);
        preg_match($re, $filename, $matches);
        if (count($matches) != 2) {
            throw new Exception(
                'Invalid chapter filename format. ' .
                'Should be of the format ch01.html (with leading zeros)'
            );
        } else {
            $html .= '<li><a class="chapter" data-url="' .
                     $webpath . '/' .$filename  . '" href="#">Chapter ' .
                     $matches[1] . '</a></li>';
        }
    }
    return $html;
}
