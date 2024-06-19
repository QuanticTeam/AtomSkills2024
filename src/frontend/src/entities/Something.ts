import { z } from 'zod'
import { apiClient } from '~/shared/apiClient'
import { UploadProps } from 'antd'

export const somethingNewSchema = z.object({
  name: z.string(),
  number: z.number(),
  integer: z.number(),
  dateTime: z.string().datetime(),
  fileKeys: z.array(z.string()),
})

export type SomethingNewDto = z.infer<typeof somethingNewSchema>

export const SomethingApi = {
  async fetch() {
    const { data } = await apiClient<Something[]>('/Something/GetAll')

    return data
  },
  async create(dto: SomethingNewDto) {
    const { data } = await apiClient.post('/Something/Create', dto)

    return data
  },
  async upload(file: UploadHandlerInfo['file'], { onProgress }: UploadOptions) {
    const payload = new FormData()

    payload.append('file', file)

    const { data } = await apiClient.post<UploadResponse>('/File/Upload', payload, {
      onUploadProgress: onProgress,
      headers: {
        'Content-Type': 'application/x-www-form-urlencoded',
      },
    })

    return data
  },
}

interface UploadOptions {
  onProgress: UploadHandlerInfo['onProgress']
}

interface UploadResponse {
  fileKey: string
  fileName: string
}

type UploadHandlerInfo = Parameters<NonNullable<UploadProps['customRequest']>>[0]

export interface Something {
  dateTime: string
  files: UploadResponse[]
  integer: number
  key: string
  name: string
  number: number
}
