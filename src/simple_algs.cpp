// Alessandro Gentilini - 2014
// Experiments for detecting anomalous bright pixels in an image

#include "cv.h"
#include "highgui.h"
#include <stdio.h>
#include <iostream>

class RunningStat
// by John D. Cook, see http://www.johndcook.com/standard_deviation.html
{
public:
    RunningStat() : m_n(0) {}

    void Clear()
    {
        m_n = 0;
    }

    void Push(double x)
    {
        m_n++;

        // See Knuth TAOCP vol 2, 3rd edition, page 232
        if (m_n == 1)
        {
            m_oldM = m_newM = x;
            m_oldS = 0.0;
        }
        else
        {
            m_newM = m_oldM + (x - m_oldM) / m_n;
            m_newS = m_oldS + (x - m_oldM) * (x - m_newM);

            // set up for next iteration
            m_oldM = m_newM;
            m_oldS = m_newS;
        }
    }

    int NumDataValues() const
    {
        return m_n;
    }

    double Mean() const
    {
        return (m_n > 0) ? m_newM : 0.0;
    }

    double Variance() const
    {
        return ( (m_n > 1) ? m_newS / (m_n - 1) : 0.0 );
    }

    double StandardDeviation() const
    {
        return sqrt( Variance() );
    }

private:
    int m_n;
    double m_oldM, m_newM, m_oldS, m_newS;
};


#include <sstream>
#include <iomanip>

#ifdef WINDOWS
#include <windows.h>
std::string timestamp()
{
    SYSTEMTIME stime;
    //structure to store system time (in usual time format)
    FILETIME ltime;
    //structure to store local time (local time in 64 bits)
    FILETIME ftTimeStamp;
    GetSystemTimeAsFileTime(&ftTimeStamp); //Gets the current system time

    FileTimeToLocalFileTime (&ftTimeStamp, &ltime); //convert in local time and store in ltime
    FileTimeToSystemTime(&ltime, &stime); //convert in system time and store in stime

    std::ostringstream oss;
    oss.fill('0');
    oss << stime.wYear
        << std::setw(2) << stime.wMonth
        << std::setw(2) << stime.wDay
        << std::setw(2) << stime.wHour
        << std::setw(2) << stime.wMinute
        << std::setw(2) << stime.wSecond
        << std::setw(3) << stime.wMilliseconds;

    return oss.str();
}
#else
std::string timestamp()
{
    time_t t = time(0);
    const int sz = 18;
    char buffer[sz] = {0};

    strftime(buffer, sz, "%Y%m%d%H%M%S000", localtime(&t));
    return std::string(buffer);
}
#endif



#include <iostream>


int main()
{
    CvCapture *capture = cvCaptureFromCAM( CV_CAP_ANY ); //CV_CAP_ANY
    if ( !capture )
    {
        fprintf( stderr, "ERROR: capture is NULL \n" );
        return -1;
    }

    cvNamedWindow( "mywindow", CV_WINDOW_AUTOSIZE );

    int width = (int)(cvGetCaptureProperty(capture, CV_CAP_PROP_FRAME_WIDTH));
    int height = (int)(cvGetCaptureProperty(capture, CV_CAP_PROP_FRAME_HEIGHT));
    double exposure = cvGetCaptureProperty(capture, CV_CAP_PROP_EXPOSURE);
    double fps = cvGetCaptureProperty(capture, CV_CAP_PROP_FPS);

    std::cerr << width << "x" << height << " exposure=" << exposure << " fps=" << fps << "\n";

    IplImage *image = cvCreateImage(cvSize(width, height), IPL_DEPTH_8U, 3);

    RunningStat rs;
    std::cout << "timestamp,minimum,maximum,mean,sd,mean_img,sd_img\n";

    IplImage *frame;
    char c;
    while ( true )
    {
        frame = cvQueryFrame( capture );
        std::string now = timestamp();
        if ( !frame )
        {
            fprintf( stderr, "ERROR: frame is null...\n" );
            getchar();
            break;
        }
        cvShowImage( "mywindow", frame );
        cvSetCaptureProperty(capture, CV_CAP_PROP_EXPOSURE, -15);
        double minp, maxp;
        cv::minMaxLoc(cv::Mat(frame), &minp, &maxp);
        cv::Mat mean_img, sd_img;
        cv::meanStdDev(cv::Mat(frame), mean_img, sd_img);
        rs.Push(maxp);
        auto mean = rs.Mean();
        auto sd = rs.StandardDeviation();
        std::cout << now << "," << minp << "," << maxp << "," << mean << "," << sd << "," << cv::mean(mean_img)[0] << "," << cv::mean(sd_img)[0] << "\n";
        if ( maxp > mean + 10 * sd )
        {
            now += ".bmp";
            cv::imwrite(now, cv::Mat(frame));
        }
    }
    cvReleaseCapture( &capture );
    cvDestroyWindow( "mywindow" );
    return 0;
}