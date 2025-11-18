#!/usr/bin/env python3
"""
Redirect & Landing Page Test Script
Tests tracking URLs, captures screenshots, and generates a PDF report.
"""

import os
import sys
from datetime import datetime
from typing import List, Dict, Optional
import requests
from playwright.sync_api import sync_playwright, Page
from reportlab.lib.pagesizes import letter
from reportlab.lib.units import inch
from reportlab.platypus import SimpleDocTemplate, Paragraph, Spacer, Image, PageBreak
from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
from reportlab.lib.enums import TA_CENTER

# Test URLs configuration
TEST_URLS = [
    {
        "name": "Default Landing Page URL",
        "url": "https://www.topxsavings.com/JN8MZMXK/HCB3C65/"
    },
    {
        "name": "Internal Article",
        "url": "https://www.topxsavings.com/JN8MZMXK/HCB3C65/?uid=2935"
    },
    {
        "name": "Original Listicle",
        "url": "https://www.topxsavings.com/JN8MZMXK/HCB3C65/?uid=3029"
    },
    {
        "name": "Dev Test",
        "url": "https://www.topxsavings.com/JN8MZMXK/HCB3C65/?uid=3090"
    }
]

# Configuration
SCREENSHOTS_DIR = "screenshots"
VIEWPORT_WIDTH = 1280
VIEWPORT_HEIGHT = 720
REQUEST_TIMEOUT = 30
PAGE_LOAD_TIMEOUT = 30000  # milliseconds


def ensure_screenshots_dir():
    """Create screenshots directory if it doesn't exist."""
    if not os.path.exists(SCREENSHOTS_DIR):
        os.makedirs(SCREENSHOTS_DIR)
        print(f"Created directory: {SCREENSHOTS_DIR}/")


def test_redirect_chain(url: str) -> Dict:
    """Test URL and capture redirect chain using requests."""
    result = {
        "success": False,
        "redirect_chain": [],
        "final_url": None,
        "final_status": None,
        "error": None
    }

    try:
        response = requests.get(url, allow_redirects=True, timeout=REQUEST_TIMEOUT)

        # Build redirect chain from history
        if response.history:
            for resp in response.history:
                result["redirect_chain"].append({
                    "url": resp.url,
                    "status": resp.status_code
                })

        # Add final response
        result["redirect_chain"].append({
            "url": response.url,
            "status": response.status_code
        })

        result["final_url"] = response.url
        result["final_status"] = response.status_code
        result["success"] = True

    except Exception as e:
        result["error"] = str(e)

    return result


def find_all_links(page: Page) -> List[Dict[str, str]]:
    """Find all unique external links on the page."""
    unique_links = {}

    try:
        # Get all links with absolute URLs (starting with http)
        links = page.locator('a[href^="http"]').all()

        print(f"    Found {len(links)} total link elements")

        for link in links:
            try:
                href = link.get_attribute('href')
                if not href or href in unique_links:
                    continue

                # Get link text and class for context
                text = link.inner_text().strip()[:100] if link.inner_text() else "(no text)"
                class_attr = link.get_attribute('class') or ''

                unique_links[href] = {
                    'url': href,
                    'text': text,
                    'class': class_attr
                }
            except Exception:
                continue

        print(f"    Found {len(unique_links)} unique external links")
        return list(unique_links.values())

    except Exception as e:
        print(f"  Error finding links: {e}")
        return []


def capture_screenshots(url: str, test_index: int, test_name: str) -> Dict:
    """Capture full-page screenshots of landing page and all linked pages."""
    result = {
        "landing_screenshot": None,
        "landing_url": None,
        "linked_pages": [],
        "error": None
    }

    # Create safe filename prefix
    prefix = f"{test_index}_{test_name.lower().replace(' ', '_').replace('-', '_')}"

    try:
        with sync_playwright() as p:
            browser = p.chromium.launch(headless=True)
            context = browser.new_context(
                viewport={'width': VIEWPORT_WIDTH, 'height': VIEWPORT_HEIGHT},
                user_agent='Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36'
            )
            page = context.new_page()

            # Navigate to the landing page
            print(f"  Loading landing page in browser...")
            try:
                page.goto(url, wait_until='networkidle', timeout=PAGE_LOAD_TIMEOUT)
            except Exception:
                # Try with domcontentloaded if networkidle times out
                page.goto(url, wait_until='domcontentloaded', timeout=PAGE_LOAD_TIMEOUT)

            # Wait for any client-side redirects or dynamic content
            page.wait_for_timeout(2000)

            landing_url = page.url
            result["landing_url"] = landing_url

            # Take FULL PAGE screenshot of landing page
            landing_screenshot_path = os.path.join(SCREENSHOTS_DIR, f"{prefix}_landing.png")
            page.screenshot(path=landing_screenshot_path, full_page=True)
            result["landing_screenshot"] = landing_screenshot_path
            print(f"  ✓ Landing page full screenshot saved: {landing_screenshot_path}")

            # Find all links on the landing page
            print(f"  Finding all links on landing page...")
            all_links = find_all_links(page)

            if not all_links:
                print(f"  ✗ No external links found on landing page")
            else:
                print(f"  Capturing screenshots of {len(all_links)} linked pages...")

                # Visit each linked page and capture screenshot
                for idx, link_info in enumerate(all_links, 1):
                    link_url = link_info['url']
                    link_text = link_info['text']

                    print(f"    [{idx}/{len(all_links)}] Loading: {link_url[:80]}...")

                    linked_page_data = {
                        'url': link_url,
                        'text': link_text,
                        'screenshot': None,
                        'status': None,
                        'error': None
                    }

                    try:
                        # Navigate to linked page
                        # Use domcontentloaded instead of networkidle for faster loading
                        try:
                            response = page.goto(link_url, wait_until='domcontentloaded', timeout=PAGE_LOAD_TIMEOUT)
                        except Exception:
                            # If that fails, try with just 'load'
                            response = page.goto(link_url, wait_until='load', timeout=PAGE_LOAD_TIMEOUT)

                        # Wait for page to render
                        page.wait_for_timeout(3000)

                        linked_page_data['status'] = response.status if response else None

                        # Take FULL PAGE screenshot
                        screenshot_path = os.path.join(SCREENSHOTS_DIR, f"{prefix}_link_{idx:03d}.png")
                        page.screenshot(path=screenshot_path, full_page=True)
                        linked_page_data['screenshot'] = screenshot_path

                        print(f"    ✓ Screenshot saved: {os.path.basename(screenshot_path)}")

                    except Exception as e:
                        error_msg = str(e)
                        # Shorten timeout errors
                        if "Timeout" in error_msg:
                            error_msg = "Timeout loading page"
                        linked_page_data['error'] = error_msg
                        print(f"    ✗ Error: {error_msg}")

                    result["linked_pages"].append(linked_page_data)

            browser.close()

    except Exception as e:
        result["error"] = str(e)
        print(f"  ✗ Error during screenshot capture: {e}")

    return result


def generate_pdf_report(test_results: List[Dict], output_file: str = "redirect_report.pdf"):
    """Generate a PDF report with all test results."""
    print("\nGenerating PDF report...")

    doc = SimpleDocTemplate(output_file, pagesize=letter,
                           leftMargin=0.75*inch, rightMargin=0.75*inch,
                           topMargin=0.75*inch, bottomMargin=0.75*inch)
    story = []
    styles = getSampleStyleSheet()

    # Custom styles
    title_style = ParagraphStyle(
        'CustomTitle',
        parent=styles['Title'],
        fontSize=24,
        textColor='#333333',
        spaceAfter=30,
        alignment=TA_CENTER
    )

    heading_style = ParagraphStyle(
        'CustomHeading',
        parent=styles['Heading1'],
        fontSize=16,
        textColor='#2c3e50',
        spaceAfter=12,
        spaceBefore=12
    )

    subheading_style = ParagraphStyle(
        'CustomSubHeading',
        parent=styles['Heading2'],
        fontSize=12,
        textColor='#34495e',
        spaceAfter=8,
        fontName='Helvetica-Bold'
    )

    body_style = ParagraphStyle(
        'CustomBody',
        parent=styles['Normal'],
        fontSize=10,
        textColor='#555555',
        spaceAfter=6
    )

    code_style = ParagraphStyle(
        'Code',
        parent=styles['Code'],
        fontSize=8,
        textColor='#2c3e50',
        fontName='Courier',
        leftIndent=20,
        spaceAfter=4
    )

    url_style = ParagraphStyle(
        'URL',
        parent=styles['Normal'],
        fontSize=9,
        textColor='#0066cc',
        fontName='Courier',
        spaceAfter=10
    )

    # Cover page
    story.append(Spacer(1, 2*inch))
    story.append(Paragraph("Redirect &amp; Landing Page Test Report", title_style))
    story.append(Paragraph("eseniorhelper.com", heading_style))
    story.append(Spacer(1, 0.5*inch))
    story.append(Paragraph(f"Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}", body_style))
    story.append(Spacer(1, 0.3*inch))
    story.append(Paragraph(f"Total Tests: {len(test_results)}", body_style))
    story.append(PageBreak())

    # Process each test result
    for idx, test_result in enumerate(test_results, 1):
        # Summary page
        story.append(Paragraph(f"Test {idx}: {test_result['name']}", heading_style))
        story.append(Spacer(1, 0.2*inch))

        story.append(Paragraph("Original Tracking URL:", subheading_style))
        story.append(Paragraph(test_result['url'], code_style))
        story.append(Spacer(1, 0.15*inch))

        # Redirect chain
        story.append(Paragraph("Redirect Chain:", subheading_style))
        if test_result['redirect_test']['success']:
            for hop_idx, hop in enumerate(test_result['redirect_test']['redirect_chain'], 1):
                story.append(Paragraph(
                    f"Hop {hop_idx}: [{hop['status']}] {hop['url']}",
                    code_style
                ))
            story.append(Spacer(1, 0.15*inch))

            story.append(Paragraph("Final Landing URL:", subheading_style))
            story.append(Paragraph(
                f"Status: {test_result['redirect_test']['final_status']}",
                body_style
            ))
            story.append(Paragraph(
                test_result['redirect_test']['final_url'],
                code_style
            ))
        else:
            story.append(Paragraph(
                f"<b>ERROR:</b> {test_result['redirect_test']['error']}",
                body_style
            ))

        story.append(Spacer(1, 0.15*inch))

        # Linked pages info
        linked_pages = test_result['screenshots']['linked_pages']
        if linked_pages:
            story.append(Paragraph(f"Linked Pages Found: {len(linked_pages)}", subheading_style))
            for link_idx, linked_page in enumerate(linked_pages, 1):
                status_text = f"[{linked_page['status']}]" if linked_page['status'] else "[ERROR]"
                story.append(Paragraph(
                    f"{link_idx}. {status_text} {linked_page['url']}",
                    code_style
                ))
        else:
            story.append(Paragraph("Linked Pages: <i>None found</i>", subheading_style))

        if test_result['screenshots']['error']:
            story.append(Spacer(1, 0.1*inch))
            story.append(Paragraph(
                f"<b>Screenshot Error:</b> {test_result['screenshots']['error']}",
                body_style
            ))

        story.append(PageBreak())

        # Landing page screenshot
        if test_result['screenshots']['landing_screenshot'] and \
           os.path.exists(test_result['screenshots']['landing_screenshot']):
            story.append(Paragraph(f"Landing Page - Test {idx}", heading_style))
            story.append(Paragraph(
                test_result['screenshots']['landing_url'] or test_result['url'],
                url_style
            ))
            story.append(Spacer(1, 0.15*inch))

            try:
                img = Image(test_result['screenshots']['landing_screenshot'])
                # Scale to fit page width and height (with margins)
                available_width = 6.5*inch
                available_height = 9*inch  # Leave room for header
                img_aspect = img.imageWidth / img.imageHeight

                # Scale by width
                scaled_width = available_width
                scaled_height = available_width / img_aspect

                # If too tall, scale by height instead
                if scaled_height > available_height:
                    scaled_height = available_height
                    scaled_width = available_height * img_aspect

                img.drawWidth = scaled_width
                img.drawHeight = scaled_height
                story.append(img)
            except Exception as e:
                story.append(Paragraph(f"<b>Error loading screenshot:</b> {e}", body_style))

            story.append(PageBreak())

        # All linked page screenshots
        for link_idx, linked_page in enumerate(test_result['screenshots']['linked_pages'], 1):
            if linked_page['screenshot'] and os.path.exists(linked_page['screenshot']):
                story.append(Paragraph(
                    f"Linked Page {link_idx}/{len(test_result['screenshots']['linked_pages'])} - Test {idx}",
                    heading_style
                ))
                story.append(Paragraph(f"Link Text: {linked_page['text']}", body_style))
                story.append(Paragraph(linked_page['url'], url_style))
                if linked_page['status']:
                    story.append(Paragraph(f"Status: {linked_page['status']}", body_style))
                story.append(Spacer(1, 0.15*inch))

                try:
                    img = Image(linked_page['screenshot'])
                    # Scale to fit page width and height (with margins)
                    available_width = 6.5*inch
                    available_height = 9*inch  # Leave room for header
                    img_aspect = img.imageWidth / img.imageHeight

                    # Scale by width
                    scaled_width = available_width
                    scaled_height = available_width / img_aspect

                    # If too tall, scale by height instead
                    if scaled_height > available_height:
                        scaled_height = available_height
                        scaled_width = available_height * img_aspect

                    img.drawWidth = scaled_width
                    img.drawHeight = scaled_height
                    story.append(img)
                except Exception as e:
                    story.append(Paragraph(f"<b>Error loading screenshot:</b> {e}", body_style))

                story.append(PageBreak())
            elif linked_page['error']:
                story.append(Paragraph(
                    f"Linked Page {link_idx}/{len(test_result['screenshots']['linked_pages'])} - Test {idx}",
                    heading_style
                ))
                story.append(Paragraph(f"Link Text: {linked_page['text']}", body_style))
                story.append(Paragraph(linked_page['url'], url_style))
                story.append(Paragraph(f"<b>Error:</b> {linked_page['error']}", body_style))
                story.append(PageBreak())

    # Build PDF
    try:
        doc.build(story)
        print(f"✓ PDF report generated: {output_file}")
        return True
    except Exception as e:
        print(f"✗ Error generating PDF: {e}")
        return False


def main():
    """Main execution function."""
    print("=" * 70)
    print("Redirect & Landing Page Test Script")
    print("=" * 70)
    print()

    # Ensure screenshots directory exists
    ensure_screenshots_dir()

    test_results = []

    # Test each URL
    for idx, test_config in enumerate(TEST_URLS, 1):
        print(f"[{idx}/{len(TEST_URLS)}] Testing: {test_config['name']}")
        print(f"  URL: {test_config['url']}")

        result = {
            "name": test_config['name'],
            "url": test_config['url'],
            "redirect_test": None,
            "screenshots": None
        }

        # Test redirect chain
        print(f"  Testing redirect chain...")
        redirect_result = test_redirect_chain(test_config['url'])
        result['redirect_test'] = redirect_result

        if redirect_result['success']:
            print(f"  ✓ Redirects followed successfully")
            print(f"    Final URL: {redirect_result['final_url']}")
            print(f"    Total hops: {len(redirect_result['redirect_chain'])}")

            # Capture screenshots
            print(f"  Capturing screenshots...")
            screenshot_result = capture_screenshots(
                redirect_result['final_url'],
                idx,
                test_config['name']
            )
            result['screenshots'] = screenshot_result

            if screenshot_result['landing_screenshot']:
                linked_count = len(screenshot_result['linked_pages'])
                successful_count = sum(1 for p in screenshot_result['linked_pages'] if p['screenshot'])
                print(f"  ✓ Captured {linked_count} linked pages ({successful_count} successful)")
            else:
                print(f"  ✗ Screenshot capture failed")
        else:
            print(f"  ✗ Redirect test failed: {redirect_result['error']}")
            result['screenshots'] = {
                "landing_screenshot": None,
                "landing_url": None,
                "linked_pages": [],
                "error": "Redirect test failed, skipped screenshots"
            }

        test_results.append(result)
        print()

    # Generate PDF report
    generate_pdf_report(test_results)

    print()
    print("=" * 70)
    print("Testing complete!")
    print(f"  Screenshots directory: {SCREENSHOTS_DIR}/")
    print(f"  PDF report: redirect_report.pdf")
    print("=" * 70)


if __name__ == "__main__":
    main()
