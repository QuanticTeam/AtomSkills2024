import { InboxOutlined } from '@ant-design/icons'
import { faker } from '@faker-js/faker'
import { zodResolver } from '@hookform/resolvers/zod'
import { Button, DatePicker, Form, Input, Space, Typography, Upload } from 'antd'
import { ReactNode, useState } from 'react'
import { Controller, SubmitHandler, useForm } from 'react-hook-form'
import { useTranslation } from 'react-i18next'
import { SomethingApi, SomethingNewDto, somethingNewSchema } from '~/entities/Something'
import { apiClient, getIsDetailedApiError } from '~/shared/apiClient'
import dayjs from 'dayjs'
import { useNavigate } from 'react-router-dom'

interface FormSomethingNewProps {
  children: ReactNode
  onSubmit: SubmitHandler<SomethingNewDto>
}

const initialValues = {
  dateTime: dayjs(faker.date.birthdate()) as any,
  fileKeys: [],
  integer: faker.number.int({ min: 1, max: 1_000_000 }),
  name: faker.animal.cat(),
  number: faker.number.float({ min: 1, max: 1_000 }),
}

export function FormSomethingNew({ children, onSubmit }: FormSomethingNewProps) {
  const { t } = useTranslation(FormSomethingNew.name)

  const {
    control,
    formState: { errors },
    handleSubmit,
    setValue,
  } = useForm<SomethingNewDto>({
    mode: 'onBlur',
    values: initialValues,
    //     resolver: zodResolver(somethingNewSchema, { async: true }),
  })

  const [submissionError, setSubmissionError] = useState('')

  return (
    <Form
      className="w-4/5 m-auto"
      labelCol={{ span: 3, offset: 3 }}
      wrapperCol={{ span: 10 }}
      layout="horizontal"
      onFinish={async (...args) => {
        try {
          await handleSubmit(onSubmit)(...args)
        } catch (error) {
          if (!getIsDetailedApiError(error)) throw error

          switch (error.code) {
            default: {
              setSubmissionError(error.code)
            }
          }
        }
      }}
    >
      <Controller
        name="name"
        control={control}
        render={({ field }) => (
          <Form.Item
            label={t('fieldName')}
            required
            help={errors.name?.message}
            validateStatus={errors.name?.message && 'error'}
          >
            <Input {...field} />
          </Form.Item>
        )}
      />

      <Controller
        name="number"
        control={control}
        render={({ field }) => (
          <Form.Item
            label={t('fieldNumber')}
            required
            help={errors.number?.message}
            validateStatus={errors.number?.message && 'error'}
          >
            <Input {...field} />
          </Form.Item>
        )}
      />

      <Controller
        name="integer"
        control={control}
        render={({ field }) => (
          <Form.Item
            label={t('fieldInteger')}
            required
            help={errors.integer?.message}
            validateStatus={errors.integer?.message && 'error'}
          >
            <Input {...field} />
          </Form.Item>
        )}
      />

      <Controller
        name="dateTime"
        control={control}
        render={({ field }) => (
          <Form.Item
            label={t('fieldDateTime')}
            required
            help={errors.integer?.message}
            validateStatus={errors.integer?.message && 'error'}
          >
            <DatePicker
              className="w-full"
              showTime
              format="DD-MM-YYYY HH:mm:ss"
              {...field}
            />
          </Form.Item>
        )}
      />

      <Controller
        name="fileKeys"
        control={control}
        render={({ field }) => (
          <Form.Item
            label={t('fieldFileKeys')}
            required
            help={errors.integer?.message}
            validateStatus={errors.integer?.message && 'error'}
          >
            <Upload.Dragger
              // This handler is invoked per file even in case of batch upload
              customRequest={async ({ file, onProgress, onSuccess, onError }) => {
                try {
                  const result = await SomethingApi.upload(file, { onProgress })

                  onSuccess?.(result)
                  setValue('fileKeys', field.value.concat(result.fileKey))
                } catch (error: any) {
                  onError?.(error) // TODO handle properly
                }
              }}
              disabled={field.disabled}
              multiple
              name={field.name}
              onChange={info => {
                switch (info.file.status) {
                  case 'done': {
                    setValue('fileKeys', [...field.value, info.file.response?.fileKey])
                    break
                  }
                  default: {
                  }
                }
              }}
            >
              {/* TODO t */}
              <p className="ant-upload-drag-icon">
                <InboxOutlined />
              </p>
              <p className="ant-upload-text">Click or drag file to this area to upload</p>
              <p className="ant-upload-hint">
                Support for a single or bulk upload. Strictly prohibited from uploading company data
                or other banned files.
              </p>
            </Upload.Dragger>
          </Form.Item>
        )}
      />

      <Form.Item
        label=" "
        colon={false}
      >
        <Space
          className="w-full"
          direction="vertical"
          classNames={{ item: 'flex justify-center' }}
        >
          <Button
            type="primary"
            htmlType="submit"
            className="w-full"
          >
            {t('submit')}
          </Button>

          {!submissionError ? null : (
            <Typography.Text type="danger">{submissionError}</Typography.Text>
          )}

          {children}
        </Space>
      </Form.Item>
    </Form>
  )
}
