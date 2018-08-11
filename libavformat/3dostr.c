/*
 * 3DO STR demuxer
 * Copyright (c) 2015 Paul B Mahol
 *
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#include "avformat.h"
#include "internal.h"

#define BASE_FREQ 240

typedef struct ThreedoDemuxContext {
    int audio_stream_index;
    int video_stream_index;
    int video_frames_count;
} ThreedoDemuxContext;

static int threedostr_probe(AVProbeData *p)
{
    if (memcmp(p->buf, "CTRL", 4) &&
        memcmp(p->buf, "SHDR", 4) &&
        memcmp(p->buf, "SNDS", 4))
        return 0;

    return AVPROBE_SCORE_MAX / 3 * 2;
}

static int threedostr_read_header(AVFormatContext *s)
{
    ThreedoDemuxContext *ctx = s->priv_data;
    unsigned chunk, codec = 0, vcodec, size, ctrl_size = -1, found_shdr = 0, found_fhdr = 0;
    AVStream *st = 0, *st2;

    ctx->audio_stream_index = -1;
    ctx->video_stream_index = -1;

    while (!avio_feof(s->pb) && (found_shdr + found_fhdr) < 2) {
        chunk = avio_rl32(s->pb);
        size  = avio_rb32(s->pb);

        if (size < 8)
            return AVERROR_INVALIDDATA;
        size -= 8;

        switch (chunk) {
        case MKTAG('C','T','R','L'):
            ctrl_size = size;
            break;
        case MKTAG('S','N','D','S'):
            if (found_shdr) {
                ++found_shdr;
                // audio only stream, rewind to beginning of the chunk
                avio_seek(s->pb, -8, SEEK_CUR);
                size = 0;
                break;
            }
            if (size < 56)
                return AVERROR_INVALIDDATA;
            avio_skip(s->pb, 8);
            if (avio_rl32(s->pb) != MKTAG('S','H','D','R'))
                return AVERROR_INVALIDDATA;
            avio_skip(s->pb, 24);

            st = avformat_new_stream(s, NULL);
            if (!st)
                return AVERROR(ENOMEM);

            ctx->audio_stream_index   = st->index;
            st->codecpar->codec_type  = AVMEDIA_TYPE_AUDIO;
            st->codecpar->sample_rate = avio_rb32(s->pb);
            st->codecpar->channels    = avio_rb32(s->pb);
            if (st->codecpar->channels <= 0)
                return AVERROR_INVALIDDATA;
            codec                  = avio_rl32(s->pb);
            avio_skip(s->pb, 4);
            if (ctrl_size == 20 || ctrl_size == 3 || ctrl_size == -1)
                st->duration       = (avio_rb32(s->pb) - 1) / st->codecpar->channels;
            else
                st->duration       = avio_rb32(s->pb) * 16 / st->codecpar->channels;
            size -= 56;
            found_shdr = 1;
            break;
        case MKTAG('S','H','D','R'):
            if (size >  0x78) {
                avio_skip(s->pb, 0x74);
                size -= 0x78;
                if (avio_rl32(s->pb) == MKTAG('C','T','R','L') && size > 4) {
                    ctrl_size = avio_rb32(s->pb);
                    size -= 4;
                }
            }
            break;
        case MKTAG('F','I','L','M'):
            if (found_fhdr) {
                ++found_fhdr;
                // video only stream, rewind to beginning of the chunk
                avio_seek(s->pb, -8, SEEK_CUR);
                size = 0;
                break;
            }
            if (size < 28)
                return AVERROR_INVALIDDATA;
            avio_skip(s->pb, 8);
            if (avio_rl32(s->pb) != MKTAG('F','H','D','R'))
                return AVERROR_INVALIDDATA;
            avio_skip(s->pb, 4);

            vcodec = avio_rl32(s->pb);
            if (vcodec != MKTAG('c','v','i','d')) {
                av_log(s, AV_LOG_WARNING, "unexpected video codec %X\n", vcodec);
                break;
            }

            st2 = avformat_new_stream(s, NULL);
            if (!st2)
                return AVERROR(ENOMEM);

            ctx->video_stream_index   = st2->index;
            st2->codecpar->codec_type = AVMEDIA_TYPE_VIDEO;
            st2->codecpar->codec_id   = AV_CODEC_ID_CINEPAK;
            st2->codecpar->codec_tag  = 0; /* no fourcc */
            st2->codecpar->height     = avio_rb32(s->pb);
            st2->codecpar->width      = avio_rb32(s->pb);
            avio_skip(s->pb, 4);
            ctx->video_frames_count   = avio_rb32(s->pb);

            avpriv_set_pts_info(st2, 33, 1, BASE_FREQ);

            size -= 36;
            found_fhdr = 1;
            break;
        case MKTAG('F','I','L','L'):
            break;
        default:
            av_log(s, AV_LOG_DEBUG, "skipping unknown chunk: %X\n", chunk);
            break;
        }

        avio_skip(s->pb, size);
    }

    if (!st)
        return 0;

    switch (codec) {
    case MKTAG('N','O','N','E'):
        st->codecpar->codec_id    = AV_CODEC_ID_PCM_S16BE;
        st->codecpar->block_align = 2 * st->codecpar->channels;
        break;
    case MKTAG('S','D','X','2'):
        st->codecpar->codec_id    = AV_CODEC_ID_SDX2_DPCM;
        st->codecpar->block_align = 1 * st->codecpar->channels;
        break;
    default:
        avpriv_request_sample(s, "codec %X", codec);
        return AVERROR_PATCHWELCOME;
    }

    avpriv_set_pts_info(st, 64, 1, st->codecpar->sample_rate);

    return 0;
}

static int threedostr_read_packet(AVFormatContext *s, AVPacket *pkt)
{
    ThreedoDemuxContext *ctx = s->priv_data;
    unsigned chunk, size, found_ssmp = 0, found_frme = 0, frme_size;
    int64_t pos, pts;
    int ret = 0;

    while (!found_ssmp && !found_frme) {
        if (avio_feof(s->pb))
            return AVERROR_EOF;

        pos   = avio_tell(s->pb);
        chunk = avio_rl32(s->pb);
        size  = avio_rb32(s->pb);

        if (!size)
            continue;

        if (size < 8)
            return AVERROR_INVALIDDATA;
        size -= 8;

        switch (chunk) {
        case MKTAG('S','N','D','S'):
            if (ctx->audio_stream_index < 0)
                break;
            if (size <= 16)
                return AVERROR_INVALIDDATA;
            avio_skip(s->pb, 8);
            if (avio_rl32(s->pb) != MKTAG('S','S','M','P'))
                return AVERROR_INVALIDDATA;
            avio_skip(s->pb, 4);
            size -= 16;
            ret = av_get_packet(s->pb, pkt, size);
            pkt->pos = pos;
            pkt->stream_index = ctx->audio_stream_index;
            pkt->duration = size / s->streams[ctx->audio_stream_index]->codecpar->channels;
            size = 0;
            found_ssmp = 1;
            break;
        case MKTAG('F','I','L','M'):
            if (ctx->video_stream_index < 0)
                break;
            if (size <= 20)
                return AVERROR_INVALIDDATA;
            pts = avio_rb32(s->pb);
            avio_skip(s->pb, 4);
            if (avio_rl32(s->pb) != MKTAG('F','R','M','E'))
                return AVERROR_INVALIDDATA;
            avio_skip(s->pb, 4); // duration
            frme_size = avio_rb32(s->pb);
            size -= 20;
            if (frme_size > size)
                return AVERROR_INVALIDDATA;
            ret = av_get_packet(s->pb, pkt, frme_size);
            pkt->pos = pos;
            pkt->stream_index = ctx->video_stream_index;
            pkt->pts = pts;
            size -= frme_size;
            found_frme = 1;
            break;
        default:
            av_log(s, AV_LOG_DEBUG, "skipping unknown chunk: %X\n", chunk);
            break;
        }

        avio_skip(s->pb, size);
    }

    return ret;
}

AVInputFormat ff_threedostr_demuxer = {
    .name           = "3dostr",
    .long_name      = NULL_IF_CONFIG_SMALL("3DO STR"),
    .priv_data_size = sizeof(ThreedoDemuxContext),
    .read_probe     = threedostr_probe,
    .read_header    = threedostr_read_header,
    .read_packet    = threedostr_read_packet,
    .extensions     = "str,cpk,cpc",
    .flags          = AVFMT_GENERIC_INDEX,
};
